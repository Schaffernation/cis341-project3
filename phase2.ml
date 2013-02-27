open Ll
open X86
open LibUtil



let compile_prog (prog : Ll.prog) : Cunit.cunit =
    let block_name = (Platform.decorate_cdecl "program") in

    (* given an instruction list, returns a list of all the uid and the *)
    (* prologue code which offset the stack accordingly *)
    let emit_uid_list (l : Ll.insn list) : uid list  =
      let rec loop (l : Ll.insn list) (accum : uid list) : uid list =
        begin match l with
        | [] -> accum
        | hd::tl -> 
          begin match hd with
          | Binop (u,_,_,_)  | Alloca u | Load (u,_) | Icmp (u,_,_,_) ->
            loop tl (u::accum)
          | _ -> loop tl (accum)
          end
        end in
    loop l [] in
      
    let get_ind (u : uid) (ul : uid  list) : ind =
      let rec index_of (elt : uid) (l : uid list) : int = 
        begin match l with
        | [] -> failwith ("not found")
        | hd::tl ->
          if hd = elt
          then 0
          else 1 + index_of elt tl
        end in
        
      let idx = index_of u ul in
      let offset = Int32.of_int (-4 * idx) in
      { i_base = Some Ebp;
        i_iscl = None;
        i_disp = Some (DImm offset) } in
    
    let emit_opnd (o : Ll.operand) (u_list : uid list) : X86.opnd =
      begin match o with
      | Local u -> Ind (get_ind u u_list)
      | Const i -> Imm i
      end in
    
    let emit_insn (i : Ll.insn) ((insn_accum, uid_l) : X86.insn list * uid list) 
                                           : X86.insn list * uid list =    
                
      let emit_binop (b : Ll.bop) : X86.insn =
        begin match b with
        | Ll.Add  -> Add (eax, ebx)
        | Ll.Sub  -> Sub (eax, ebx)
        | Ll.Mul  -> Imul(Eax, ebx)
        | Ll.Shl  -> Shl (eax, ebx)
        | Ll.Lshr -> Shr (eax, ebx)
        | Ll.Ashr -> Sar (eax, ebx)
        | Ll.And  -> And (eax, ebx)
        | Ll.Or   -> Or  (eax, ebx)
        | Ll.Xor  -> Xor (eax, ebx)
        end in
        
      let emit_icmp (i : Ll.cmpop) : X86.cnd = 
        begin match i with
        | Ll.Eq   -> Eq
        | Ll.Ne   -> NotEq
        | Ll.Slt   -> Slt
        | Ll.Sle   -> Sle
        | Ll.Sgt   -> Sgt
        | Ll.Sge   -> Sge
        end in
      
      begin match i with
      | Binop (u, b, o1, o2) ->
        let loc = Ind (get_ind u uid_l) in
        let insn1 = Mov (ebx, emit_opnd o2 uid_l) in
        let insn2 = Mov (eax, emit_opnd o1 uid_l) in
        let binop = emit_binop b :: [(Mov (loc, eax))] in
        
        (insn1 :: insn2 :: binop @ insn_accum, uid_l)
      | Alloca u -> (insn_accum, uid_l)
      | Load (u, o) -> 
        let loc = Ind (get_ind u uid_l) in
   			
				(Mov (loc, emit_opnd o uid_l) :: insn_accum, uid_l)
      | Store (o1, o2) ->
        (Mov (emit_opnd o2 uid_l, emit_opnd o1 uid_l) :: insn_accum, uid_l)
          
      | Icmp (u, c, o1, o2) -> 
        let loc = Ind (get_ind u uid_l) in
        let cmpr  = 
					Cmp (emit_opnd o1 uid_l, emit_opnd o2 uid_l) :: 
					Setb (loc, (emit_icmp c)) :: 
					[] in
        
        (cmpr @ insn_accum, uid_l)
				
      end in
      
    let emit_bblock (b : Ll.bblock) (bl_accum : insn_block list) : insn_block list =
      let uid_l = emit_uid_list b.Ll.insns in
      let b_insns, uid_l = List.fold_right emit_insn b.Ll.insns ([], uid_l) in
      
      (* cdecl stuff *)
      let offset = Int32.of_int (4 * List.length uid_l) in
      let prologue = 
        (Push (ebp)) ::
        (Mov (ebp, esp)) ::
        (Add (esp, Imm (Int32.neg offset))) ::
        [] in
      let epilogue = 
        (Add (esp, Imm offset)) ::
        (Pop (ebp)) :: 
        [] in
        
      let b_end =
				epilogue @
        begin match b.terminator with
        | Ll.Ret op  -> Mov (eax, emit_opnd op uid_l) :: [Ret]
        | Ll.Br l   ->  [Jmp (Lbl l)]
        | Cbr (op, l1, l2) ->
					[Cmp (emit_opnd op uid_l, Imm 0l)] @
          [J (Eq, l1)] @
          [J (NotEq, l2)]
        end in
      
      let new_blk = {
        global = true;
        label = b.Ll.label;
        insns = prologue @ b_insns @ b_end
      } in
      
      (new_blk :: bl_accum) in
      
    let emit_bblocks (b : Ll.bblock list) : insn_block list =
      List.fold_right emit_bblock b [] in
      
    let blk_list = emit_bblocks prog.ll_cfg in
    
    let rec mk_strt (ls : insn_block list) : insn_block list =
      begin match ls with
      | [] -> []
      | hd::tl -> 
        if hd.label = prog.ll_entry
        then 
          { global = true;
            label = mk_lbl_named block_name;
            insns = hd.X86.insns
            } :: mk_strt tl
        else mk_strt tl
      end in
    
    let blk_list = mk_strt blk_list in
    
    List.map (fun blk -> Cunit.Code blk) blk_list
    
    
      
    
      
      
      
      
      