open Ll
open X86
open LibUtil



let compile_prog (prog : Ll.prog) : Cunit.cunit =
    let block_name = (Platform.decorate_cdecl "program") in
		
		(* let rec get_block (entry : Ll.lbl) (bblist : bblock list) : Ll.bblock = *)
		(* 	begin match bblist with                                               *)
		(* 	| [] -> failwith "lbl not here"                                       *)
		(* 	| blk::rest ->                                                        *)
		(* 		if entry = blk.label                                                *)
		(* 		then blk                                                            *)
		(* 		else get_block entry rest                                           *)
		(* 	end in                                                                *)
			
		let emit_bblock (b : Ll.bblock) (bl_accum : insn_block list) : insn_block list =
			let b_insns, map = List.fold_right emit_insn b.insns ([],[]) in
		
			let b_end =
  			begin match b.terminator with
  			| Ll.Ret op	-> emit_opnd op map @ [Ret]
  			| Ll.Br l 	-> [Jmp (Lbl l)]
  			| Cbr (op, l1, l2) ->
					emit_opnd op map @
					[J (Eq    (l1))] @
					[J (NotEq (l2))]
  			end in
  		
			let new_blk = {
				global = true;
				label = b.label;
				insns = b_insns @ b_end
			} in
			
			(new_blk @ bl_accum) in
			
		let emit_bblocks (b : Ll.bblock list) : Cunit.component list =
			List.fold_right emit_bblock b [] in

		(* Generate a stack offset *)
		let mk_bp_offset : unit -> ind =
			let ctr = ref 0 in
      fun _ -> incr ctr;
				 {i_base = Some Esp;
          i_iscl = None;
          i_disp = Some (DImm (-4l * !ctr))} in


		let emit_insn (i : Ll.insn) ((insn_accum, uid_map) : X86.insn list * ((uid * ind) list)) 
																					 : X86.insn list * ((uid * ind) list) =		
																						
			let emit_binop (b : Ll.bop) : X86.insn =
				begin match b with
				| Ll.Add  -> Add (eax, ebx)
				| Ll.Sub  -> Sub (eax, ebx)
				| Ll.Mul  -> Imul(Eax, ebx)
				| Ll.Shl  -> Shl (eax, ebx)
				| Ll.Lshr -> Shr (eax, ebx)
				| Ll.Ashr -> Sar (eax, ebx)
				| Ll.And  -> And (eax, ebx)
				| Ll.Or 	-> Or  (eax, ebx)
				| Ll.Xor  -> Xor (eax, ebx)
				end in
				
			let emit_icmp (i : Ll.cmpop) : X86.cnd = 
				begin match i with
				| Ll.Eq 	-> Eq
				| Ll.Ne 	-> NotEq
				| Ll.Slt 	-> Slt
				| Ll.Sle 	-> Sle
				| Ll.Sgt 	-> Sgt
				| Ll.Sge 	-> Sge
				end in
			
			begin match i with
			| Binop (u, b, o1, o2) ->
				let loc = List.assoc u uid_map in
				let insn1 = emit_opnd o2 uid_map @ [(Mov (ebx, eax))] in
				let insn2 = emit_opnd o1 uid_map in
				let binop = emit_binop b :: [(Mov (loc, eax))] in
				
				(insn1 @ insn2 @ binop, uid_map)
			| Alloca u -> 
				let up_insns = (Add (esp, Imm -4l))::insn_accum in
				let up_uid_map = (u, Ind (mk_bp_offset ()))::uid_map in
				
				(up_insns, up_uid_map)
			| Load (u, o) -> 
				let loc = List.assoc u uid_map in
				let up_insn = emit_opnd o uid_map @ [(Mov (loc,eax))] @ insn_accum in
				
				(up_insns, uid_map)
			| Store (o1, o2) ->
				let insn1 = emit_opnd o1 uid_map in
				let insn2 = 
					begin match o2 with
					| Local u -> 
						let loc = List.assoc u uid_map in
						[(Mov (loc,eax))]
					| Const i -> [(Mov (Imm i,eax))]
					end in
				(insn1 @ insn2, uid_map)
					
			| Icmp (u, c, o1, o2) -> 
				let loc = List.assoc u uid_map in
				let insn1 = emit_opnd o2 uid_map @ [(Mov (ebx, eax))] in
				let insn2 = emit_opnd o1 uid_map @ [(Cmp (eax, ebx))] in
				let cmpr  = Setb (eax, (emit_icmp c)) :: [] in
				
				(insn1 @ insn2 @ cmpr, uid_map)
			end in
			
		(* puts answer in EAX *)
		let emit_opnd (o : Ll.operand) (uid_map : ((uid * ind) list)) : X86.insn list =
			begin match o with
			| Local u -> 
				let loc = List.assoc u uid_map in
				[(Mov (eax, loc))]
			| Const i ->
				[(Mov (eax, Imm i))]
			end in
			
		let blk_list = emit_bblocks prog.ll_cfg in
		List.map (fun blk -> Cunit.Code blk) blk_list
			
		
			
			
			
			
			