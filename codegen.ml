module L = Llvm
module A = Ast
open Sast

module StringMap = Map.Make(String)
module Hash = Hashtbl

let global_classes:(string, L.lltype) Hash.t = Hash.create 50
let local_params:(string, L.llvalue) Hash.t = Hash.create 50
let local_values:(string, L.llvalue) Hash.t = Hash.create 50
let class_self:(string, L.llvalue) Hash.t = Hash.create 50
let class_fields:(string, int) Hash.t = Hash.create 50

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
      pix_t; i32_t; i32_t; i32_t; i32_t; |] in
  let frame_t     = L.struct_type context [| (*ToDo: add placements arr *)
      i32_t; i32_t; |] in

  let the_module = L.create_module context "PixCzar" in

  let rec ltype_of_typ = function
      A.Int       -> i32_t
    | A.Void      -> void_t
    | A.String    -> str_t
    | A.Float     -> float_t
    | A.Bool      -> i1_t
    | A.Null      -> i32_t
    | A.Array(t)  -> L.pointer_type (ltype_of_typ t)
    (*| A.Pix
    | A.Placement ->
    | A.Frame ->*)
    | t -> raise (Failure ("Type " ^ A.string_of_typ t ^ " not implemented yet"))
  in

  (* Declare each global variable; remember its value in a map *)
  let global_vars : L.llvalue StringMap.t =
    let global_var m (t, n) =
      let init = match t with
          A.Float -> L.const_float (ltype_of_typ t) 0.0
        | _ -> L.const_int (ltype_of_typ t) 0
      in StringMap.add n (L.define_global n init the_module) m in
    List.fold_left global_var StringMap.empty globals in

  (* declare i32 @printf(i8*, ...) *)
  let printf_t : L.lltype =
      L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let printf_func : L.llvalue =
     L.declare_function "printf" printf_t the_module in

  let to_imp str = raise (Failure ("Not yet implemented1: " ^ str)) in
  let to_imp2 str = raise (Failure ("Not yet implemented2: " ^ str)) in
  let to_imp3 str = raise (Failure ("Not yet implemented3: " ^ str)) in
  let to_imp4 str = raise (Failure ("Not yet implemented4: " ^ str)) in
  let to_imp5 str = raise (Failure ("Not yet implemented5: " ^ str)) in

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

  (* Fill in the body of the given function *)
  let build_function_body fdecl =
    let (the_function, _) = StringMap.find fdecl.sfname function_decls in
    let builder = L.builder_at_end context (L.entry_block the_function) in

    let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder
    and float_format_str = L.build_global_stringptr "%g\n" "fmt" builder
    and string_format_str = L.build_global_stringptr "%s\n" "fmt" builder in

    (* Construct the function's "locals": formal arguments and locally
       declared variables.  Allocate each on the stack, initialize their
       value, if appropriate, and remember their values in the "locals" map *)
    let local_vars =
      let add_formal m (t, n) p =
        let () = L.set_value_name n p in
	let local = L.build_alloca (ltype_of_typ t) n builder in
        let _  = L.build_store p local builder in
	StringMap.add n local m
      in

      (* Allocate space for any locally declared variables and add the
       * resulting registers to our map *)
      let add_local m ((t, n), _) =
	let local_var = L.build_alloca (ltype_of_typ t) n builder
	in StringMap.add n local_var m
      in

      let formals = List.fold_left2 add_formal StringMap.empty fdecl.sformals
          (Array.to_list (L.params the_function)) in
      List.fold_left add_local formals fdecl.slocals
    in

    let lookup n = try StringMap.find n local_vars
                   with Not_found -> StringMap.find n global_vars
    in

    let rec expr builder ((_, _, e) : sexpr) = match e with
        (* 42  ----->  i32 42 *)
        SLiteral i -> L.const_int i32_t i
      | SStringLit st -> L.build_global_stringptr st "tmp" builder
      | SFliteral l -> L.const_float float_t l
      | SBoolLit b -> L.const_int i1_t (if b then 1 else 0)
      | SNoexpr -> L.const_int i32_t 0
(*
      | SId s -> L.build_load (lookup s) s builder
*)
      | SId s -> id_gen builder s true
(*
      | SAssign (e1, e2) -> let e' = expr builder e1 in
          let check_var = match e1 with
             (_, _, SId(s)) -> let _ = Hash.add params s (expr builder e2) in e'
           | _ -> to_imp2 "SAssign type"
          in check_var
*)
      | SAssign(e1, e2) -> assign_gen builder e1 e2
      | SCall ("printf", [e]) ->
        L.build_call printf_func [| string_format_str ; (expr builder e) |] "printf" builder
      | SBinop (e1, op, e2) -> binop_gen builder e1 op e2
      | SUnop(op, e) -> unop_gen builder op e
      | SNullLit -> L.const_null i32_t
(*
      | SNewArray(t, size) -> new_array_gen builder t, size
*)
      | SCreateArray(el) -> create_array_gen builder el
      | SAccessArray(name, idx) -> access_array_gen builder name idx false
(* not needed
      | SPostUnop of sexpr * post_uop
      | SAssign of sexpr * sexpr
      | SCall of string * sexpr list
      | SNew of typ * sexpr list
      | SNewArray of typ * int
      | SCreateArray of sexpr list
      | SSubArray of string * int * int
      | SAccessStruct of string * string
      | SPostIncrement of sexpr
      | SPostDecrement of sexpr
*)
      | _ -> to_imp ""

    and id_gen builder id is_deref =
(*
      let () = Printf.printf "id_gen %s %B \n%!" id is_deref in
*)
      if is_deref then
        if Hash.mem local_params id then
          Hash.find local_params id
        else
          if Hash.mem local_values id then
            let _val = Hash.find local_values id in
            L.build_load _val id builder
          else
          raise(Failure("Unknown variable deref " ^ id))
      else
(*
      let () = Printf.printf "notderef %s %B \n%!" id is_deref in
*)
        if Hash.mem local_values id then
          Hash.find local_values id
        else if Hash.mem local_params id then
          Hash.find local_params id
        else
          raise(Failure("Unknown variable nonderef " ^ id))

    and assign_gen builder se1 se2 =
      let (_, t1, e1) = se1 in
      let (_, t2, e2) = se2 in

      let (lhs, is_obj_access) = match e1 with
          SId id -> (id_gen builder id false, false)
        | SAccessArray(name, idx) -> (access_array_gen builder name idx true, true)
        | _ -> raise(Failure("Unable to assign " ^ string_of_sexpr se1 ^ " to "
                             ^ string_of_sexpr se2))
      in

      let rhs = match e2 with
          SId(id) -> (match t2 with
              A.Pix | A.Placement | A.Frame | A.String
              (* | A.Array | A.Struct *) -> id_gen builder id false
            | _ -> id_gen builder id true)
        | SAccessArray(name, idx) -> access_array_gen builder name idx true
        | _ -> expr builder se2
      in

      let rhs = match t2 with
          A.Pix | A.Placement | A.Frame | A.String
          (* | A.Array | A.Struct *) -> if is_obj_access then rhs
                                else L.build_load rhs "tmp" builder
        | A.Null -> L.const_null (ltype_of_typ t2)
        | _ -> rhs
      in

      ignore(L.build_store rhs lhs builder);
      rhs

    and create_array_gen builder el =
      let e = List.hd el in
      let (m, t, sx) = e in
      let lt = ltype_of_typ t in

      (* This will not work for arrays of objects *)
      let size = (expr builder e) in
      let size_t = L.build_intcast (L.size_of lt) i32_t "tmp" builder in
      let size = L.build_mul size_t size "tmp" builder in
      let size_real = L.build_add size (L.const_int i32_t 1) "arr_size" builder in

      let arr = L.build_array_malloc lt size_real "tmp" builder in
      let arr = L.build_pointercast arr (L.pointer_type lt) "tmp" builder in

      let arr_len_ptr = L.build_pointercast arr (L.pointer_type i32_t) "tmp" builder in

      (* Store length at this position *)
      ignore(L.build_store size_real arr_len_ptr builder);
      (* initialise_array arr_len_ptr size_real (const_int i32_t 0) 0 builder; *)
      arr

    and access_array_gen builder name index is_assign =
      let arr_obj = id_gen builder name true in
      let sindex = expr builder index in
      let sindex = L.build_add sindex (L.const_int i32_t 1) "list_index" builder in
      let _val = L.build_gep arr_obj [| sindex |] "list_access" builder in
      if is_assign then
        _val
      else
        L.build_load _val "list_access_val" builder

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
      and (_, t, _) = e in
      let build_unop op unop_typ lval = match op, unop_typ with
          A.Neg, A.Int -> L.build_neg lval "neg_int_tmp" builder
        | A.Neg, A.Float -> L.build_fneg lval "neg_flt_tmp" builder
        | A.Not, A.Bool -> L.build_not lval "not_bool_tmp" builder
        | _ -> raise(Failure("Unsupported unop for " ^ A.string_of_uop op ^
          " and type " ^ A.string_of_typ t))
      in

      match t with
          A.Int | A.Float | A.Bool -> build_unop unop t unop_lval
        | _ -> raise(Failure("Invalid type for unop: " ^ A.string_of_typ t))
        in

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
                            | _ -> to_imp4 (A.string_of_typ fdecl.styp)
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
      | SVarDecs(svar_list) ->
        let svar_dec_gen svar =
          let ((t, s), e) = svar in
          let lltype = ltype_of_typ t in
          let alloca = L.build_alloca lltype s builder in
            Hash.add local_values s alloca;
          let (_, _, sx) = e in
            match sx with
                SAssign(lhs, rhs) -> assign_gen builder lhs rhs
              | _ -> alloca in
        svar_dec_gen (List.nth svar_list 0); builder (* for now just do 1st one *)

      | SIf (predicate, then_stmt, elseif_stmts, else_stmt) ->
         let bool_val = expr builder predicate in
	 let merge_bb = L.append_block context "merge" the_function in
         let branch_instr = L.build_br merge_bb in

	 let then_bb = L.append_block context "then" the_function in
         let then_builder = stmt (L.builder_at_end context then_bb) then_stmt in
	 let () = add_terminal then_builder branch_instr in

	 let else_bb = L.append_block context "else" the_function in
         let else_builder = stmt (L.builder_at_end context else_bb) else_stmt in
	 let () = add_terminal else_builder branch_instr in

	 let _ = L.build_cond_br bool_val then_bb else_bb builder in
	 L.builder_at_end context merge_bb
      | s -> to_imp (string_of_sstmt (map, ss))
    in

      (* Build the code for each statement in the function *)
    let builder = stmt builder (StringMap.empty, SBlock fdecl.sbody) in

    (* Add a return if the last block falls off the end *)
    add_terminal builder (match fdecl.styp with
        A.Void -> L.build_ret_void
      | A.Float -> L.build_ret (L.const_float float_t 0.0)
      | t -> L.build_ret (L.const_int (ltype_of_typ t) 0))

  in List.iter build_function_body functions; the_module
