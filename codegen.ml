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
  let pix_t       = L.struct_type context [|
      (* Currently only holding width, height, need rgb*)
      i32_t; i32_t; |] in
  let placement_t = L.struct_type context [|
      L.pointer_type (pix_t); i32_t; i32_t; i32_t; i32_t; |] in
  let frame_t = L.struct_type context [|
      i32_t; i32_t; L.pointer_type (placement_t) |] in

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
  let builtin_printf_t : L.lltype =
      L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let builtin_printf_func : L.llvalue =
     L.declare_function "printf" builtin_printf_t the_module in
  let builtin_render_t : L.lltype =
      L.var_arg_function_type i32_t [||] in
  let builtin_render_func : L.llvalue =
     L.declare_function "render" builtin_render_t the_module in
      

  let to_imp str = raise (Failure ("Not yet implemented1: " ^ str)) in

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
     
    let gen_default_value t builder = match t with
        A.Int -> L.const_int i32_t 0
      | A.Float -> L.const_float float_t 0.0
      | A.Bool -> L.const_int i1_t 0
      | A.String -> L.build_global_stringptr "" "tmp" builder
      | _ -> raise(Failure("No default value for this type")) in

    let rec expr builder ((m, t, e) : sexpr) = match e with
        SLiteral i -> L.const_int i32_t i
      | SStringLit st -> L.build_global_stringptr st "tmp" builder
      | SFliteral l -> L.const_float float_t l
      | SBoolLit b -> L.const_int i1_t (if b then 1 else 0)
      | SNoexpr -> L.const_int i32_t 0
      | SId s -> id_gen builder s true
      | SAssign(e1, e2) -> assign_gen builder e1 e2
      | SCall (id, e) -> (match id with
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
         | "render"  -> 
             L.build_call builtin_render_func [||] "render" builder
         | _ -> if StringMap.mem id function_decls then
                  let (the_function, fdecl) = StringMap.find id function_decls in
                  let build_expr_list expr_list e = (expr builder e) :: expr_list in
                  let arg_list = Array.of_list(List.rev(
                      List.fold_left build_expr_list [] e)) in
                  if fdecl.styp = Void then
                    L.build_call the_function arg_list "" builder else
                    L.build_call the_function arg_list id builder
                else raise(Failure("Built-in function not implemented"))
        )
      | SBinop (e1, op, e2) -> binop_gen builder e1 op e2
      | SUnop(op, e) -> unop_gen builder op e
      | SNullLit -> L.const_null i32_t
      | SNew(t, el) -> (match t with
          Pix         -> L.build_malloc (ltype_of_typ t) "pix_create" builder
          | _ -> to_imp "Additional types")

      | SNewArray(t, size) -> let lt = ltype_of_typ t 
        in create_array_gen builder lt size
      | SCreateArray(el) -> let e = List.hd el in let (_, t, _) = e in
         let lt = ltype_of_typ t in
         let arr = create_array_gen builder lt (List.length el)
         in let _ = fill_array builder arr (List.rev el) in arr
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
      | _ -> to_imp "statement"

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
          raise(Failure("Unknown variable" ^ id))

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
          SId id -> let lhs = id_gen builder id false in
            ignore(L.build_store rhs lhs builder)
        | SAccessArray(name, idx) -> let lhs = access_array_gen builder name idx
            true in ignore(L.build_store rhs lhs builder)
        | _ -> raise(Failure("Unable to assign " ^ string_of_sexpr se1 ^ " to "
                             ^ string_of_sexpr se2))) in
      rhs

    and create_array_gen builder lt size =
      let size_arr =  L.const_int i32_t (size+1) in (*Including space to store size*)
      let arr = L.build_array_malloc lt size_arr "array_gen" builder in
      L.build_pointercast arr (L.pointer_type lt) "array_cast" builder

    and fill_array builder arr el = 
      let array_assign idx arr_e = ignore(L.build_store (expr builder arr_e) 
        (L.build_gep arr [| (L.const_int i32_t (idx+1)) |] "array_assign"
        builder) builder) in 
      List.iteri array_assign el;

    and llvm_int_to_int llint =
      let llint = L.int64_of_const llint in match llint with
          Some(x) -> Int64.to_int(x)
        | _ -> raise(Failure("int64 operation failed"))

    and access_array_gen builder name index is_assign =
      let arr = id_gen builder name true in
      let index = expr builder index in
      let int_index = llvm_int_to_int index in
      let int_size = Hash.find array_info name in
      let _ = (if int_index < 0 || int_index >= int_size
            then raise(Failure("Illegal index"))) in
      
      let index = L.build_add index (L.const_int i32_t 1) "add" builder in
      let arr_val = L.build_gep arr [| index |] "arr_access" builder in
      if is_assign then arr_val
      else L.build_load arr_val "arr_access_val" builder
    
    and binop_gen  builder e1 op e2 =
      let (_, t, _) = e1
        and e1' = expr builder e1
        and e2' = expr builder e2
        in if t = A.Float then (match op with
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
          ) e1' e2' "tmp" builder
        else (match op with
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
      let svar = List.hd svar_list in
      let ((t, s), (m, et, e')) = svar in
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
      let _ = ignore(L.build_store svar' alloca builder) in builder in

 let build_global svar_list = 
     let svar = List.hd svar_list in
      let ((t, s), _) = svar in
      let lltype = ltype_of_typ t in
      ignore(Hash.add global_values s (L.declare_global lltype s the_module)) in

  let _ = List.iter build_global globals in 


  (* Fill in the body of the given function *)
  let build_function_body fdecl =
    let (the_function, _) = StringMap.find fdecl.sfname function_decls in
    let builder = L.builder_at_end context (L.entry_block the_function) in

    let _ = (if fdecl.sfname="main" then
        let declare_globals svar_list = 
          let svar = List.hd svar_list in
          let ((t, s), (m', t',e')) = svar in
          let svar' = if t'=A.Void then
            gen_default_value t builder else expr builder (m', t',e') in
          ignore(Hash.add global_values s (L.define_global s svar' the_module))
        in List.iter declare_globals globals) in


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
      | None -> ignore (instr builder) in
(*
  This function generates code for statements
*)
    let rec stmt builder (map, ss) = match ss with
        SExpr e -> let _ = expr builder e in builder
      | SBlock sl -> List.fold_left stmt builder sl
      | SReturn e -> let _ = match fdecl.styp with
                              A.Int -> L.build_ret (expr builder e) builder
                            | _ -> to_imp (A.string_of_typ fdecl.styp)
                     in builder
      | SWhile (predicate, body) ->
          (* First create basic block for condition instructions -- this will
          serve as destination in the case of a loop *)
        let pred_bb = L.append_block context "while" the_function in
              (* In current block, branch to predicate to execute the condition *)
        let _ = L.build_br pred_bb builder in

              (* Create the body's block, generate the code for it, and add a branch
              back to the predicate block (we always jump back at the end of a while
              loop's body, unless we returned or something) *)
        let body_bb = L.append_block context "while_body" the_function in
              let while_builder = stmt (L.builder_at_end context body_bb) body in
        let () = add_terminal while_builder (L.build_br pred_bb) in

              (* Generate the predicate code in the predicate block *)
        let pred_builder = L.builder_at_end context pred_bb in
        let bool_val = expr pred_builder predicate in

              (* Hook everything up *)
        let merge_bb = L.append_block context "merge" the_function in
        let _ = L.build_cond_br bool_val body_bb merge_bb pred_builder in
        L.builder_at_end context merge_bb

      | SFor (e1, e2, e3, body) -> stmt builder
	    (map, ( SBlock [(map, SExpr e1) ; (map, SWhile (e2, (map, SBlock [body ;
            (map, SExpr e3)]))) ]))
      | SVarDecs(svar_list) -> (*TODO: multiple declarations in a line *)
            build_vars svar_list local_values builder
      | SIf (predicate, then_stmt, elseif_stmts, else_stmt) ->
        if_gen predicate then_stmt elseif_stmts else_stmt
      | SElseIf (_, _) -> raise(Failure("Should never reach SElseIf"))
      | s -> to_imp (string_of_sstmt (map, ss))

    and if_gen predicate then_stmt elseif_stmts else_stmt =
      let if_bool_val = expr builder predicate in
      let if_bb = L.append_block context "if" the_function in
      let () = add_terminal builder (L.build_br if_bb) in
      
      let merge_bb = L.append_block context "merge" the_function in
      let branch_instr = L.build_br merge_bb in

      let then_bb = L.append_block context "then" the_function in
      let then_builder = stmt (L.builder_at_end context then_bb) then_stmt in
      let () = add_terminal then_builder branch_instr in
      
      let else_bb = L.append_block context "else" the_function in
      let else_builder = stmt (L.builder_at_end context else_bb) else_stmt in
      let () = add_terminal else_builder branch_instr in

      let rec elseif_bb_gen elseif_list bool_val pred_bb body_bb = match elseif_list with
           (_, SElseIf(pred, body)) :: tl -> 
             let elseif_pred_val = expr (L.builder_at_end context pred_bb) pred in
             let elseif_pred_bb = L.append_block context "elseif_pred" the_function in
             
             let elseif_body_bb = L.append_block context "elseif_body" the_function in
             let elseif_builder = stmt (L.builder_at_end context elseif_body_bb) body in
             let () = add_terminal elseif_builder branch_instr in

             let _ = L.build_cond_br bool_val body_bb elseif_pred_bb
                (L.builder_at_end context pred_bb)
             in elseif_bb_gen tl elseif_pred_val elseif_pred_bb elseif_body_bb

         | _ -> L.build_cond_br bool_val body_bb else_bb 
                (L.builder_at_end context pred_bb)
       in let elseif_ss = match elseif_stmts with 
           (_, SBlock(elseif_ss)) -> elseif_ss
         | _ -> raise(Failure("Elseif must contain a Block"))
       
      in let _ = elseif_bb_gen elseif_ss if_bool_val if_bb then_bb in
       L.builder_at_end context merge_bb
    in
      (* Build the code for each statement in the function *)
    let builder = stmt builder (StringMap.empty, SBlock fdecl.sbody) in

    (* Add a return if the last block falls off the end *)
    add_terminal builder (match fdecl.styp with
        A.Void -> L.build_ret_void
      | A.Float -> L.build_ret (L.const_float float_t 0.0)
      | t -> L.build_ret (L.const_int (ltype_of_typ t) 0))

  in List.iter build_function_body functions; the_module
