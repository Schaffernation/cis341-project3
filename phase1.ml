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
		let new_uid = Ll.mk_uid "" in
		(new_uid, [Ll.Store (op, Ll.Local new_uid)])
		
	| Ast.Binop (b, e1, e2) -> 
		(* Compile e1 and e2 *)		
		let (op1, insn1) = decode_op e1 c in
		let (op2, insn2) = decode_op e2 c in

		let new_uid = Ll.mk_uid "" in
		let binop_insn = compile_binop b new_uid op1 op2 in
		(new_uid, insn1 @ insn2 @ [binop_insn])
		
	| Ast.Unop (u, e1) -> 
		let (op1, insn1) = decode_op e1 c in
		
		let new_uid = Ll.mk_uid "" in
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

let rec compile_var_decls (vars : Ast.var_decl list) : Ll.insn list * Ctxt.t =
	let folder (var : Ast.var_decl) ((insns, ctxt) : Ll.insn list * Ctxt.t ) =
		let up_ctxt, uid = Ctxt.alloc var.Ast.v_id ctxt in
		let (exp_op, exp_insn) = decode_op var.Ast.v_init ctxt in
		let up_insns = exp_insn @ Ll.Alloca(uid)::Ll.Store(exp_op,Ll.Local(uid))::insns in
		(up_insns, up_ctxt)
	in List.fold_right folder vars ([], Ctxt.empty)

	and compile_stmt (s : Ast.stmt) (c : Ctxt.t) : Ll.insn list * Ctxt.t = 
  	begin match s with
  	| Assign (l, e) -> 
  	| If (e, s1, s2op) ->
  	| While (e, s1) ->
  	| For (vs, eop, s1op, s2)
  	| Block b -> compile_block b c
  	end
		
	and compile_block (b : Ast.block) (c : Ctxt.t) : Ll.insn list * Ctxt.t =

let compile_prog ((block, ret):Ast.prog) : Ll.prog =
	(* let vdecls, stmts = block in *)
	failwith "blah"
				
				
				