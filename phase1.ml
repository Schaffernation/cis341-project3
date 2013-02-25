open LibUtil

let (>::) x y = y :: x
(* 
 * Parse and AST from a lexbuf 
 * - the filename is used to generate error messages
 *)
let parse (filename : string) (buf : Lexing.lexbuf) : Ast.prog =
  try
    Lexer.reset_lexbuf filename buf;
    Parser.toplevel Lexer.token buf
  with Parsing.Parse_error ->
    failwithf  "Parse error at %s." (Range.string_of_range (Lexer.lex_range buf))


(* 
 * Compile a source binop in to an LL instruction.
 *)
let compile_binop (b : Ast.binop) : Ll.uid -> Ll.operand -> Ll.operand -> Ll.insn  =
  let ib b id op1 op2 = (Ll.Binop (id, b, op1, op2)) in
  let ic c id op1 op2 = (Ll.Icmp (id, c, op1, op2)) in
  match b with
  | Ast.Plus  -> ib Ll.Add
  | Ast.Times -> ib Ll.Mul
  | Ast.Minus -> ib Ll.Sub
  | Ast.And   -> ib Ll.And
  | Ast.Or    -> ib Ll.Or
  | Ast.Shl   -> ib Ll.Shl
  | Ast.Shr   -> ib Ll.Lshr
  | Ast.Sar   -> ib Ll.Ashr

  | Ast.Eq    -> ic Ll.Eq
  | Ast.Neq   -> ic Ll.Ne
  | Ast.Lt    -> ic Ll.Slt
  | Ast.Lte   -> ic Ll.Sle
  | Ast.Gt    -> ic Ll.Sgt
  | Ast.Gte   -> ic Ll.Sge

let compile_unop (u : Ast.unop) (id: Ll.uid) (op : Ll.operand) : Ll.insn =
	begin match u with
	| Ast.Neg    -> (Ll.Binop (id, Ll.Mul, op, Ll.Const (-1l)))
	| Ast.Lognot -> (Ll.Icmp  (id, Ll.Eq,  op, Ll.Const (0l )))
	| Ast.Not 	 -> (Ll.Binop (id, Ll.Xor, op, Ll.Const (-1l)))
	end
	
let compile_opnd (e : Ast.exp) (c : Ctxt.t) : Ll.operand option =
	begin match e with
	| Ast.Cint i -> Some (Ll.Const i)
	| Ast.Id s -> Some (Ll.Local (Ctxt.lookup s c))
	| _ -> None
	end

(* Simplifies exp in a given context to an instruction and its exiting uid *)
let rec compile_exp (e : Ast.exp) (c : Ctxt.t) : Ll.uid * Ll.insn list =
	begin match e with
	| Ast.Cint _ | Ast.Id _ -> 
		let op = 
  		begin match compile_opnd e c with
  		| None -> failwith "wtf"
  		| Some o -> o
  		end in
		let new_uid = Ll.gen_sym () in
		(new_uid, [Ll.Store (op, Ll.Local new_uid)])
		
	| Ast.Binop (b, e1, e2) -> 
		(* Compile e1 and e2 *)		
		let (op1, insn1) = decode_op e1 c in
		let (op2, insn2) = decode_op e2 c in

		let new_uid = Ll.gen_sym () in
		let binop_insn = compile_binop b new_uid op1 op2 in
		(new_uid, insn1 @ insn2 @ [binop_insn])
		
	| Ast.Unop (u, e1) -> 
		let (op1, insn1) = decode_op e1 c in
		
		let new_uid = Ll.gen_sym () in
		let unop_insn = compile_unop u new_uid op1 in
		(new_uid, insn1 @ [unop_insn])
	end
	
	(* Simplifies input exp to Some (Cint or an Id) or None if exp is anything else *)
  and decode_op (e : Ast.exp) (c : Ctxt.t) : Ll.operand * Ll.insn list =
  	let op = compile_opnd e c in
  	begin match op with
  	| None -> 
  			let (uid1, insn1) = compile_exp e c in
  			(Ll.Local uid1, insn1)
  	| Some o -> (o, [])
  	end 

(** Mutating Context Recursion Block **)

(* Returns a list of instructions and the context that those instructions generate *)
let rec compile_var_decls (vars : Ast.var_decl list) (old_ctxt : Ctxt.t) : Ll.insn list * Ctxt.t =
	let folder (var : Ast.var_decl) ((insns, ctxt) : Ll.insn list * Ctxt.t ) =
		let up_ctxt, uid = Ctxt.alloc var.Ast.v_id ctxt in
		let (exp_op, exp_insn) = decode_op var.Ast.v_init ctxt in
		let up_insns = exp_insn @ Ll.Alloca(uid)::Ll.Store(exp_op,Ll.Local(uid))::insns in
		(up_insns, up_ctxt)
	in List.fold_right folder vars ([], old_ctxt)

	and compile_stmt (s : Ast.stmt) (c : Ctxt.t) (next_l : Ll.lbl) : Ll.lbl * Ll.bblock list =		 
		begin match s with
  	| Ast.Assign (l, e) ->
			let l_uid =
				begin match l with
				| Ast.Var str -> (Ctxt.lookup str c)
				end in
			let (op1, insn1) = decode_op e c in
			let b_ilist = insn1 @ [Ll.Store (op1, Ll.Local l_uid)] in
			let b_lbl = Ll.mk_lbl () in
			let b_term = Ll.Br next_l in
			(b_lbl, [{ Ll.label = b_lbl; Ll.insns = b_ilist; Ll.terminator = b_term }])
			
  	| Ast.If (e, s1, s2op) ->
			let bool_uid, cmp_insns = compile_exp e c in
			let s1_lbl, s1_bl_list = compile_stmt s1 c next_l in
			let s2_lbl, s2_bl_list = 
				begin match s2op with
				| None -> 
					let new_lbl = Ll.mk_lbl () in
					let new_bl = {Ll.label = new_lbl; 
												Ll.insns = []; 
												Ll.terminator = Ll.Br next_l } in
					(new_lbl, [new_bl])
				| Some s2 -> compile_stmt s2 c next_l
				end in
			let b_term = Ll.Cbr (Ll.Local (bool_uid), s1_lbl, s2_lbl) in
			let b_lbl = Ll.mk_lbl () in
			let if_block = 
				{ Ll.label = b_lbl; 
				  Ll.insns = cmp_insns; 
					Ll.terminator = b_term } in
			(b_lbl, [if_block] @ s1_bl_list @ s2_bl_list)
			
  	(* | Ast.While (e, s1) ->           *)
  	(* | Ast.For (vs, eop, s1op, s2) -> *)
  	| Ast.Block b -> 
			let b_lbl, b_bl, b_ctxt = compile_block b c next_l in
			(b_lbl,b_bl)
		| _ -> failwith "workin on it"
  	end
	
	(* Returns a tuple with the a bblock list and its topmost label *)
	and compile_stmts (s : Ast.stmt list) (c : Ctxt.t) (next : Ll.lbl) : Ll.lbl * Ll.bblock list =
		let block_list, next_l = 
  		List.fold_right (
  			fun (st : Ast.stmt) ((bblist, next_lbl) : Ll.bblock list * Ll.lbl) ->
  				let s_lbl, s_blk = compile_stmt st c next_lbl in
  				(s_blk @ bblist, s_lbl)
  				) s ([], next) in
		(next_l, block_list)
		
	(* Takes an Ast.Block and its context and returns a block list of its compiled innards, *)
	(* and a label pointing to the top of the block list and the outer context it generates *)
	and compile_block (b : Ast.block) (outer_ctxt : Ctxt.t) (next : Ll.lbl) : Ll.lbl * Ll.bblock list * Ctxt.t =
		let vars, stmts = b in
		let entry_insns, inner_ctxt = compile_var_decls vars outer_ctxt in
		let top_lbl, bblist = compile_stmts stmts inner_ctxt next in
		let b_lbl = Ll.mk_lbl () in
		let entry_block =
			{Ll.label = b_lbl;
			 Ll.insns = entry_insns;
			 Ll.terminator = Ll.Br top_lbl } in
		(b_lbl, entry_block::bblist, outer_ctxt)

let compile_prog ((block, ret):Ast.prog) : Ll.prog =
	let finish_lbl = Ll.mk_lbl () in
	let strt_lbl, block_list, block_ctxt = compile_block block Ctxt.empty finish_lbl in
	let finish_uid, finish_insns =  compile_exp ret block_ctxt in
	let finish_block = 
	{ Ll.label = finish_lbl;
		Ll.insns = finish_insns;
		Ll.terminator = Ll.Ret (Ll.Local (finish_uid)) } in
	{ Ll.ll_cfg = block_list @ [finish_block] ;
	  Ll.ll_entry = strt_lbl }
		
	
				
				
				