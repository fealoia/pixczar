module L = Llvm
module A = Ast
open Sast

module StringMap = Map.Make(String)
module Hash = Hashtbl

let local_values:(string, L.llvalue) Hash.t = Hash.create 50
let global_values:(string, L.llvalue) Hash.t = Hash.create 50
let array_info:(string, int) Hash.t = Hash.create 50

(* Code Generation from the SAST. Returns an LLVM module if successful,
   throws an exception if something is wrong. *)
let translate (globals, functions) =
  let context    = L.global_context () in
  (* Add types to the context so we can use them in our LLVM code *)
  let i32_t       = L.i32_type    context
  and i8_t        = L.i8_type     context
  and void_t      = L.void_type   context
  and str_t       = L.pointer_type (L.i8_type context)
  and float_t     = L.double_type context
  and i1_t        = L.i1_type     context in

  let pix_struct   = L.named_struct_type context "pix" in
  let () = L.struct_set_body pix_struct [|i32_t; str_t; i32_t; i32_t; L.pointer_type
  i32_t|] false in
  let pix_t = L.pointer_type pix_struct in

  let placement_struct   = L.named_struct_type context "placement" in
    let () = L.struct_set_body placement_struct
      [| pix_t; i32_t; i32_t |] false in
  let placement_t = L.pointer_type placement_struct in
  let placement_node = L.named_struct_type context "placement_node" in
  let placement_node_t = L.pointer_type placement_node in
    let () = L.struct_set_body placement_node
      [| placement_node_t; placement_t |] false in

  let frame_struct   = L.named_struct_type context "frame" in
    let () = L.struct_set_body frame_struct [|placement_node_t;|] false in
  let frame_t = L.pointer_type frame_struct in

  let the_module = L.create_module context "PixCzar" in

  let rec ltype_of_typ = function
      A.Int       -> i32_t
    | A.Void      -> void_t
    | A.String    -> str_t
    | A.Float     -> float_t
    | A.Bool      -> i1_t
    | A.Null      -> i32_t
    | A.Array(t, _)  -> L.pointer_type (ltype_of_typ t)
    | A.Pix       -> pix_t
    | A.Placement -> placement_t
    | A.Frame     -> frame_t
    | t -> raise (Failure ("Type " ^ A.string_of_typ t ^ " not implemented yet"))
  in

  (* declare built-in functions *)
  let builtin_exit_t : L.lltype =
      L.var_arg_function_type void_t [||] in
  let builtin_exit_func : L.llvalue =
     L.declare_function "exit" builtin_exit_t the_module in
  let builtin_printf_t : L.lltype =
      L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let builtin_printf_func : L.llvalue =
     L.declare_function "printf" builtin_printf_t the_module in
  let builtin_render_t : L.lltype =
      L.var_arg_function_type i32_t [| i32_t; L.pointer_type frame_t; i32_t; i32_t;
      i32_t; |] in
  let builtin_render_func : L.llvalue =
     L.declare_function "render" builtin_render_t the_module in

  let to_imp str = raise (Failure ("Not yet implemented: " ^ str)) in

  (* Define each function (arguments and return type) so we can
   * define it's body and call it later *)
  let function_decls : (L.llvalue * sfunc_decl) StringMap.t =
    let function_decl m fdecl =
      let name = fdecl.sfname
      and formal_types =
	Array.of_list (List.map (fun (t,_) -> ltype_of_typ t) fdecl.sformals)
      in let ftype = L.function_type (ltype_of_typ fdecl.styp) formal_types in
      StringMap.add name (L.define_function name ftype the_module, fdecl) m in
    List.fold_left function_decl StringMap.empty functions in

    let int_format_str builder = L.build_global_stringptr "%d\n" "fmt" builder in
    let string_format_str builder = L.build_global_stringptr "%s\n" "fmt" builder in
    let float_format_str builder = L.build_global_stringptr "%g\n" "fmt" builder in
    let bool_format_str builder = L.build_global_stringptr "%d\n" "fmt" builder in

    let fill_struct structobj el builder =
       let store_el idx e =
           let e_p = L.build_struct_gep structobj idx "struct_build" builder
           in ignore(L.build_store e e_p builder)
       in List.iteri store_el el in

    let typ_malloc typ_ptr typ_struct el_arr builder =
       let struct_malloc = L.build_malloc typ_struct "malloc" builder in
       let struct_malloc = L.build_pointercast struct_malloc typ_ptr "cast"
         builder in
       let () = fill_struct struct_malloc el_arr builder
       in struct_malloc in
    
    let create_array_gen builder lt size =
      let size_arr =  L.const_int i32_t (size+1) in
      let arr = L.build_array_malloc lt size_arr "array_gen" builder in
      let arr = L.build_pointercast arr (L.pointer_type lt) "array_cast"
      builder in
      let casted = L.build_gep arr [|L.const_int i32_t 0|] "size"
        builder in
      let casted = L.build_pointercast casted (L.pointer_type i32_t) "cast"
      builder in
      let _ = ignore(L.build_store (L.const_int i32_t size)
      casted builder) in arr in
    
    let rec gen_default_value t builder = let zero = L.const_int i32_t 0 in 
    match t with
        A.Int -> L.const_int i32_t 0
      | A.Float -> L.const_float float_t 0.0
      | A.Bool -> L.const_int i1_t 0
      | A.String -> L.build_global_stringptr "" "tmp" builder
      | A.Pix -> typ_malloc pix_t pix_struct
           [zero;L.const_pointer_null str_t;zero;zero;
           L.const_pointer_null (L.pointer_type i32_t)] builder
      | A.Placement -> let pix = gen_default_value A.Pix builder in
           typ_malloc placement_t placement_struct [pix; zero;zero]
           builder
      | A.Frame -> let node = typ_malloc placement_node_t placement_node
           [L.const_pointer_null placement_node_t; L.const_pointer_null
             placement_t] builder in
           typ_malloc frame_t frame_struct [node] builder
      | A.Array(typ,_) -> create_array_gen builder (ltype_of_typ typ) 0
      | _ -> raise(Failure("No default value for this type")) in
      
    let rec expr builder ((m, t, e) : sexpr) = match e with
        SLiteral i -> L.const_int i32_t i
      | SStringLit st -> L.build_global_stringptr st "tmp" builder
      | SFliteral l -> L.const_float float_t l
      | SBoolLit b -> L.const_int i1_t (if b then 1 else 0)
      | SNoexpr -> L.const_int i32_t 0
      | SId s -> id_gen builder s true
      | SAssign(e1, e2) -> assign_gen builder e1 e2
      | SCall (id, e) ->
         let build_expr_list expr_list e = (expr builder e) :: expr_list in
         (match id with
           "printf" ->
             L.build_call builtin_printf_func [| float_format_str builder ; (expr
                builder (List.hd e)) |] "printf" builder
         | "printi"  ->
             L.build_call builtin_printf_func [| int_format_str builder ; (expr builder
                (List.hd e)) |] "printf" builder
         | "prints"  ->
             L.build_call builtin_printf_func [| string_format_str builder ; (expr builder
                (List.hd e)) |] "printf" builder
         | "printb"  ->
             L.build_call builtin_printf_func [| bool_format_str builder ; (expr builder
                (List.hd e)) |] "printf" builder
         | "length" -> let (_,t,ss) = List.hd e in (match ss with
             SId(s) -> (match t with
                Array(t,_) -> let arr = id_gen builder s true in
                  let ptr = L.build_gep arr [|L.const_int i32_t 0|] "size" builder in
                  let size = L.build_bitcast ptr (L.pointer_type i32_t) "cast"
                    builder in 
                  L.build_load size "size" builder
              | _ -> raise(Failure("Incorrect length input")))
            | SNewArray(_,idx) -> L.const_int i32_t idx
            | SCreateArray(el) -> L.const_int i32_t (List.length el)
            | _ -> raise(Failure("incorrect type")))
         | "render"  -> let size = (match List.hd e with
            | (_,A.Array(_,size),_) -> size
            | _ -> raise(Failure("Invalid render input"))) in
             let arg_list = (List.rev (List.fold_left build_expr_list
               [L.const_int i32_t (size)] e)) in
             L.build_call builtin_render_func (Array.of_list arg_list) "render" builder
         | _ -> if StringMap.mem id function_decls then
                  let (the_function, fdecl) = StringMap.find id function_decls in
                  let arg_list = Array.of_list
                    (List.rev(List.fold_left build_expr_list [] e)) in
                  if fdecl.styp = Void then
                    L.build_call the_function arg_list "" builder else
                    L.build_call the_function arg_list id builder
                else raise(Failure("Built-in function not implemented"))
        )
      | SBinop (e1, op, e2) -> binop_gen builder e1 op e2
      | SUnop(op, e) -> unop_gen builder op e
      | SNullLit -> L.const_null i32_t
      | SNew(t, el) -> let rec to_ll ll_list el_list = (match el_list with
          hd :: tl -> to_ll ((expr builder hd) :: ll_list) tl
        | _ -> List.rev(ll_list)) in let arr = to_ll [] el in
          (match t with (*ToDo: garbage collection*)
          Pix -> gen_default_value A.Pix builder
        | Placement -> typ_malloc placement_t placement_struct arr builder
        | Frame -> gen_default_value A.Frame builder
        | _ -> to_imp "Additional types")
      | SNewArray(t, size) -> let lt = ltype_of_typ t in
        let arr = create_array_gen builder lt size in
        let rec fill size = (match size with
           0 -> ()
         | _ -> let _ = ignore(L.build_store (gen_default_value t builder)
           (L.build_gep arr [| (L.const_int i32_t size) |] "array_assign"
           builder) builder) in fill (size-1)) in 
           let _ = fill (size) in arr
      | SCreateArray(el) -> let e = List.hd el in let (_, t, _) = e in
         let lt = ltype_of_typ t in
         let arr = create_array_gen builder lt (List.length el) in
         let _ = fill_array builder arr (List.rev el) in arr
      | SAccessArray(name, idx) -> access_array_gen builder name idx false
      | SPostUnop(e, op) -> let (_, _, e'') = e in let e' = expr builder e in (match op with
            PostIncrement -> (match e'' with
              SId(_) | SAccessArray(_, _) ->
                assign_gen builder e (m, t, SBinop(e, A.Add, (m, t, SLiteral(1))))
              | _ -> L.build_add e' (L.const_int i32_t 1) "add" builder)
          | PostDecrement -> (match e'' with
              SId(_) | SAccessArray(_, _) ->
                assign_gen builder e (m, t, SBinop(e, A.Sub, (m, t, SLiteral(1))))
              | _ -> L.build_sub e' (L.const_int i32_t 1) "sub" builder)
      )
      | _ -> to_imp "expression"
    
    and id_gen builder id deref =
        if Hash.mem local_values id then
            let _val = Hash.find local_values id in
            if deref = true then
              L.build_load _val id builder
            else _val
        else if Hash.mem global_values id then
            let _val = Hash.find global_values id in
            if deref = true then
              L.build_load _val id builder
            else _val
        else
          raise(Failure("Unknown variable: " ^ id))

    and assign_gen builder se1 se2 =
      let (_, t1, e1) = se1 in
      let (_, t2, e2) = se2 in

      let rhs = (match e2 with
          SId(id) -> id_gen builder id true
        | SAccessArray(name, idx) -> access_array_gen builder name idx true
        | _ -> expr builder se2) in
      let rhs = (match t2 with
          A.Null -> L.const_null (ltype_of_typ t2)
        | _ -> rhs) in
      let _ = (match e1 with
          SId id -> let _ = (match e2 with 
             SCreateArray(el) -> ignore(Hash.add array_info id (List.length el))
           | SNewArray(_,s) -> ignore(Hash.add array_info id s)
           | _ -> ()) in let lhs = id_gen builder id false in
            ignore(L.build_store rhs lhs builder)
        | SAccessArray(name, idx) -> let lhs = access_array_gen builder name idx
            true in ignore(L.build_store rhs lhs builder)
        | _ -> raise(Failure("Unable to assign " ^ string_of_sexpr se1 ^ " to "
                             ^ string_of_sexpr se2))) in
      rhs

    and fill_array builder arr el =
      let array_assign idx arr_e = ignore(L.build_store (expr builder
      arr_e)
      (L.build_gep arr [| (L.const_int i32_t (idx+1)) |] "array_assign"
        builder) builder) in
      List.iteri array_assign el

    and llvm_int_to_int llint =
      let llint = L.int64_of_const llint in match llint with
          Some(x) -> Int64.to_int(x)
        | _ -> raise(Failure("int64 operation failed"))
    
    and access_array_gen builder name index is_assign =
      let (_,_,ss) = index in
      let arr = id_gen builder name true in
      let index = expr builder index in
      let _ = (match ss with
         SLiteral(i) ->
          let int_index = llvm_int_to_int index in
          let int_size = Hash.find array_info name in
          (if int_index < 0 || int_index >= int_size
            then raise(Failure("Illegal index")))
        | _ -> ()) in
      let (check,_) = StringMap.find "check_access" function_decls in
      let size = L.build_gep arr [| L.const_int i32_t 0 |] "size" builder in
      let size = L.build_pointercast size (L.pointer_type i32_t) "cast" builder in
      let size = L.build_load size "size" builder in
      let _ = L.build_call check [|index; size|] "" builder in
      let index = L.build_add index (L.const_int i32_t 1) "add" builder in
      let arr_val = L.build_gep arr [| index |] "arr_access" builder in
      if is_assign then arr_val
      else L.build_load arr_val "arr_access_val" builder

    and f_op op = match op with
        A.Add     -> L.build_fadd
      | A.Sub     -> L.build_fsub
      | A.Mult    -> L.build_fmul
      | A.Div     -> L.build_fdiv
      | A.Equal   -> L.build_fcmp L.Fcmp.Oeq
      | A.Neq     -> L.build_fcmp L.Fcmp.One
      | A.Less    -> L.build_fcmp L.Fcmp.Olt
      | A.Leq     -> L.build_fcmp L.Fcmp.Ole
      | A.Greater -> L.build_fcmp L.Fcmp.Ogt
      | A.Geq     -> L.build_fcmp L.Fcmp.Oge
      | A.And | A.Or | A.Mod ->
          raise (Failure "internal error: semant should have rejected and/or on float")

    and binop_gen builder e1 op e2 =
      let (_, t1, _) = e1 and (_, t2, _) = e2
        and e1' = expr builder e1
        and e2' = expr builder e2 in
        if t1=t2 && t1 = A.Float then (f_op op) e1' e2' "tmp" builder
        else if t1=t2 then (match op with
          | A.Add     -> L.build_add
          | A.Sub     -> L.build_sub
          | A.Mult    -> L.build_mul
          | A.Div     -> L.build_sdiv
          | A.Mod     -> L.build_srem
          | A.And     -> L.build_and
          | A.Or      -> L.build_or
          | A.Equal   -> L.build_icmp L.Icmp.Eq
          | A.Neq     -> L.build_icmp L.Icmp.Ne
          | A.Less    -> L.build_icmp L.Icmp.Slt
          | A.Leq     -> L.build_icmp L.Icmp.Sle
          | A.Greater -> L.build_icmp L.Icmp.Sgt
          | A.Geq     -> L.build_icmp L.Icmp.Sge
          ) e1' e2' "tmp" builder
       else 
          (f_op op) (if t1=Int then L.build_sitofp e1' float_t "cast" builder else e1')
          (if t2=Int then L.build_sitofp e2' float_t "cast" builder else e2') "tmp" builder

      and unop_gen builder unop e =
      let unop_lval = expr builder e
      and (m, t, e') = e in match unop, t with
          A.Neg, A.Int -> L.build_neg unop_lval "neg_int_tmp" builder
        | A.Neg, A.Float -> L.build_fneg unop_lval "neg_flt_tmp" builder
        | A.Not, A.Bool -> L.build_not unop_lval "not_bool_tmp" builder
        | A.PreIncrement, _ -> (match e' with
             SId(_) | SAccessArray(_, _) -> let _ = assign_gen builder e (m, t, SBinop(e,
                A.Add, (m, t, SLiteral(1)))) in unop_lval
          | _ -> let _ = L.build_add unop_lval (L.const_int i32_t 1) "add" builder
             in unop_lval)
        | A.PreDecrement, _ -> (match e' with
             SId(_) | SAccessArray(_, _) -> let _ = assign_gen builder e (m, t, SBinop(e,
                A.Sub, (m, t, SLiteral(1)))) in unop_lval
          | _ -> let _ = L.build_sub unop_lval (L.const_int i32_t 1) "add" builder
             in unop_lval)
        | _ -> raise(Failure("Unsupported unop for " ^ A.string_of_uop unop ^
          " and type " ^ A.string_of_typ t))
        in

  let build_vars svar_list hashtable builder =
      let ((t,_),_) = List.hd svar_list in
      let build_var builder svar =
          let ((_, s), (m, et, e')) = svar in
          let svar' = if et=A.Void then
              gen_default_value t builder else
              expr builder (m, et, e') in
          let lltype = ltype_of_typ t in
          let alloca = L.build_alloca lltype s builder in
          let _ = (match t with
              Array(_,size) -> Hash.add array_info s (match et with
              Array(_,et_size) -> et_size
                | _ -> 0)
              | _ -> ()) in
          let _ = Hash.add hashtable s alloca in
          let _ = ignore(L.build_store svar' alloca builder) in builder
       in List.fold_left build_var builder svar_list in

 let build_global svar_list =
     let build_svar svar =
      let ((t, s), _) = svar in
      let lltype = ltype_of_typ t in
      ignore(Hash.add global_values s (L.declare_global lltype s the_module)) in
     List.iter build_svar svar_list in

  let _ = List.iter build_global globals in

  let bool_pred_gen builder e = let (_,t,sx) = e in
    let e' = expr builder e in (match t with
        A.Bool -> e'
      | A.Int -> L.build_icmp L.Icmp.Sgt e' (L.const_int i32_t 0) "tmp" builder
      | A.Float -> L.build_fcmp L.Fcmp.Ogt e' (L.const_float float_t 0.0) "tmp" builder
      | _ -> (match sx with
          SNullLit -> L.const_int i1_t 0
        | _ -> L.const_int i1_t 1)) in

  (* Fill in the body of the given function *)
  let build_function_body fdecl =
    let (the_function, _) = StringMap.find fdecl.sfname function_decls in
    let builder = L.builder_at_end context (L.entry_block the_function) in

    let _ = (if fdecl.sfname="main" then
        let declare_globals svar_list =
          let ((t,_),_) = List.hd svar_list in
          let declare_svar svar =
          let ((_, s), (m', t',e')) = svar in
          let svar' = if t'=A.Void then
            gen_default_value t builder else expr builder (m', t',e') in
          ignore(Hash.add global_values s (L.define_global s svar' the_module))
          in List.iter declare_svar svar_list
        in List.iter declare_globals globals) in

     let func_params idx (t,s) = let _val = L.param the_function idx in
       let alloca = L.build_alloca (L.type_of _val) "param_alloc" builder in
       let _ = ignore(L.build_store _val alloca builder) in
       let _ = (match t with
         A.Array(_,size) -> ignore(Hash.add array_info s (Int32.to_int(Int32.max_int)))
         | _ -> ()) in
       ignore(Hash.add local_values s alloca) in
     let _ = List.iteri func_params fdecl.sformals in
    
     let _ = (if fdecl.sfname="check_access" then
      let checked_block = L.append_block context "checked" the_function in
      let _ = L.build_ret_void (L.builder_at_end context checked_block) in
      let exit_block = L.append_block context "exit" the_function in
      let exit_block_b = L.builder_at_end context exit_block in
      let str = L.build_global_stringptr "Throwing Runtime Error: Out of Bound Array Access" "tmp" builder in
      let _ = L.build_call builtin_printf_func [| string_format_str builder ;
          str|] "printf" exit_block_b in 
      let _ = L.build_call builtin_exit_func [||] "" exit_block_b in
      let _ = L.build_ret_void exit_block_b in
      
      let idx = L.param the_function 0 in
      let size = L.param the_function 1 in
      let cmp = L.build_icmp L.Icmp.Sgt size idx "tmp" builder in
      ignore(L.build_cond_br cmp checked_block exit_block builder)) in

    (* Each basic block in a program ends with a "terminator" instruction i.e.
    one that ends the basic block. By definition, these instructions must
    indicate which basic block comes next -- they typically yield "void" value
    and produce control flow, not values *)
    (* Invoke "instr builder" if the current block doesn't already
       have a terminator (e.g., a branch). *)
    let add_terminal builder instr =
                           (* The current block where we're inserting instr *)
      match L.block_terminator (L.insertion_block builder) with
        Some _ -> ()
      | None -> (match instr with 
          Some(instr) -> ignore (instr builder)
        | None -> raise(Failure("No default return value for this type"))) in
    
    let rec stmt builder (map, ss) loop_list = match ss with
        SExpr e -> let _ = expr builder e in builder
      | SBlock sl -> let stmt_block builder s = stmt builder s loop_list in
          List.fold_left stmt_block builder sl
      | SReturn e -> let _ = match fdecl.styp with
          A.Void -> L.build_ret_void builder
        | _ -> L.build_ret (expr builder e) builder
        in builder
      | SWhile (predicate, body) ->
          (* First create basic block for condition instructions -- this will
          serve as destination in the case of a loop *)
        let pred_bb = L.append_block context "while" the_function in
              (* In current block, branch to predicate to execute the condition *)
        let _ = L.build_br pred_bb builder in
        let merge_bb = L.append_block context "merge" the_function in

              (* Create the body's block, generate the code for it, and add a branch
              back to the predicate block (we always jump back at the end of a while
              loop's body, unless we returned or something) *)
        let body_bb = L.append_block context "while_body" the_function in
         let while_builder = stmt (L.builder_at_end context body_bb) body
              ((pred_bb, merge_bb) :: loop_list) in
         let () = add_terminal while_builder (Some(L.build_br pred_bb)) in
              (* Generate the predicate code in the predicate block *)
        let pred_builder = L.builder_at_end context pred_bb in
        let bool_val = bool_pred_gen pred_builder predicate in

        let _ = L.build_cond_br bool_val body_bb merge_bb pred_builder in
        L.builder_at_end context merge_bb

      | SFor (e1, e2, e3, body) -> stmt builder
	    (map, ( SBlock [(map, SExpr e1) ; (map, SWhile (e2, (map, SBlock [body ;
            (map, SExpr e3)]))) ])) loop_list
      | SVarDecs(svar_list) ->
            build_vars svar_list local_values builder
      | SIf (predicate, then_stmt, elseif_stmts, else_stmt) ->
              if_gen builder predicate then_stmt elseif_stmts else_stmt loop_list
      | SElseIf (_, _) -> raise(Failure("Should never reach SElseIf"))
      | SBreak -> let () = add_terminal builder (Some(L.build_br (snd (List.hd
      loop_list)))) in builder
      | SContinue -> let () = add_terminal builder (Some (L.build_br (fst (List.hd
      loop_list)))) in builder
      | SObjCall(e, name, el) -> let build_expr_list expr_list e =
          (expr builder e) :: expr_list in (match name with
          "addPlacement" -> let frame = expr builder e in
             let placement = expr builder (List.hd el) in
             let pnode_ptr = L.build_struct_gep frame 0 "add_plcmt" builder in
             let prev_node = L.build_load pnode_ptr "node" builder in
             let node = typ_malloc placement_node_t placement_node
               [prev_node; placement] builder in
             let _ = ignore(L.build_store node pnode_ptr builder) in
             builder
         | "clearPlacements" -> let frame = expr builder e in
             let node_ptr = L.build_struct_gep frame 0 "clear_plcmts" builder in
             let _ = ignore(L.build_store (L.const_pointer_null
             placement_node_t) node_ptr builder) in
             let plcmt_ptr = L.build_struct_gep frame 1 "clear_plcmts" builder in
             let _ = ignore(L.build_store (L.const_pointer_null
             placement_t) plcmt_ptr builder) in builder
         | "makeRectangle" -> let pix = expr builder e in
             let _ = fill_struct pix
             (List.rev(List.fold_left build_expr_list
             [L.const_pointer_null str_t; L.const_int i32_t 1] el)) builder
             in builder
         | "makeTriangle" -> let pix = expr builder e in
             let _ = fill_struct pix
             (List.rev(List.fold_left build_expr_list [L.const_int i32_t 0;
             L.const_pointer_null str_t; L.const_int i32_t 2]
               el)) builder
             in builder
         | "makeEllipse" -> let pix = expr builder e in
             let _ = fill_struct pix
             (List.rev(List.fold_left build_expr_list
             [L.const_pointer_null str_t; L.const_int i32_t 3] el)) builder
             in builder
         | "uploadImage" -> let pix = expr builder e in
             let _ = fill_struct pix (List.rev(List.fold_left
             build_expr_list [L.const_int i32_t 4] el)) builder in builder
         | "clear"-> let pix = expr builder e in
             let _ = fill_struct pix (List.rev(List.fold_left
             build_expr_list [L.const_int i32_t 0] el)) builder in builder
         | _ -> let _ = to_imp "object call: " ^ name in builder
      )
      | s -> to_imp (string_of_sstmt (map, ss))

    and if_gen builder predicate then_stmt elseif_stmts else_stmt loop_list =
      let if_bool_val = bool_pred_gen builder predicate in
      let if_bb = L.append_block context "if" the_function in
      let () = add_terminal builder (Some (L.build_br if_bb)) in

      let merge_bb = L.append_block context "merge" the_function in
      let branch_instr = L.build_br merge_bb in

      let then_bb = L.append_block context "then" the_function in
      let then_builder = stmt (L.builder_at_end context then_bb) then_stmt
        loop_list in
      let () = add_terminal then_builder (Some branch_instr) in

      let else_bb = L.append_block context "else" the_function in
      let else_builder = stmt (L.builder_at_end context else_bb) else_stmt
        loop_list in
      let () = add_terminal else_builder (Some branch_instr) in

      let rec elseif_bb_gen elseif_list bool_val pred_bb body_bb loop_list = match elseif_list with
           (_, SElseIf(pred, body)) :: tl ->
             let elseif_pred_val = expr (L.builder_at_end context pred_bb) pred in
             let elseif_pred_bb = L.append_block context "elseif_pred" the_function in

             let elseif_body_bb = L.append_block context "elseif_body" the_function in
             let elseif_builder = stmt (L.builder_at_end context elseif_body_bb)
               body loop_list in
             let () = add_terminal elseif_builder (Some branch_instr) in

             let _ = L.build_cond_br bool_val body_bb elseif_pred_bb
                (L.builder_at_end context pred_bb)
             in elseif_bb_gen tl elseif_pred_val elseif_pred_bb elseif_body_bb loop_list

         | _ -> L.build_cond_br bool_val body_bb else_bb
                (L.builder_at_end context pred_bb)
       in let elseif_ss = match elseif_stmts with
           (_, SBlock(elseif_ss)) -> elseif_ss
         | _ -> raise(Failure("Elseif must contain a Block"))

       in let _ = elseif_bb_gen elseif_ss if_bool_val if_bb then_bb loop_list in
       L.builder_at_end context merge_bb
    in
      (* Build the code for each statement in the function *)
    let builder = stmt builder (StringMap.empty, SBlock fdecl.sbody) [] in
      
    (* Add a return if the last block falls off the end *)
    add_terminal builder (match fdecl.styp with
        A.Void -> (Some L.build_ret_void)
      | A.Float -> (Some (L.build_ret (L.const_float float_t 0.0)))
      | A.Int -> (Some (L.build_ret (L.const_int i32_t 0)))
      | A.Bool -> (Some (L.build_ret (L.const_int i1_t 0)))
      | t -> None)

    in List.iter build_function_body functions; the_module
