open Ast
open Sast

module StringMap = Map.Make(String)

let check (globals, functions) =
  (* Check if a certain kind of binding has void type or is a duplicate
     of another, previously checked binding *)
  let check_binds (kind : string) (to_check : bind list) =
    let check_it checked binding =
      let void_err = "illegal void " ^ kind ^ " " ^ snd binding
      and dup_err = "duplicate " ^ kind ^ " " ^ snd binding
      in match binding with
        (* No void bindings *)
        (Void, _) -> raise (Failure void_err)
      | (_, n1) -> match checked with
                    (* No duplicate bindings *)
                      ((_, n2) :: _) when n1 = n2 -> raise (Failure dup_err)
                    | _ -> binding :: checked
    in let _ = List.fold_left check_it [] (List.sort compare to_check)
       in to_check
  in let get_first lst = match lst with
      hd :: tl -> hd
    | _ -> ((Notyp, ""), Noexpr) in
 
  let rec add_types typed_list var_list = (match var_list with
        ((_,s),e) :: tl -> let t = fst (fst (List.hd var_list)) in
          add_types (((t,s),e) :: typed_list) tl
      | _ -> typed_list) in

  let get_binds var_list_list =
     let fold_types combine var_list =
         let var_list = List.rev (add_types [] var_list) in
         let rec fold_list combine var_list = (match var_list with
              ((t,s),_) :: tl -> fold_list ((t,s) :: combine) tl
            | _ -> combine)
         in fold_list combine var_list in
     List.fold_left fold_types [] var_list_list in

      (* Check vars to see if duplicate or void type *)
  let check_vars (kind : string) (to_check: var list list) =
      let combined_list = get_binds to_check in
      let _ = ignore(check_binds kind combined_list)
    in to_check
  in

  let map_to_svar id typ resultlist = ((typ, id), (StringMap.empty, typ,
    SNoexpr)) :: resultlist in
  let globals' = check_vars "global" globals in

  (* Collect function declarations for built-in functions: no bodies *)
  let built_in_decls =
    let add_bind map (name, typ, formal_vars) = StringMap.add name {
      typ = typ; fname = name;
      formals = formal_vars; locals = []; body = [] } map
    in List.fold_left add_bind StringMap.empty [("render", Int,
    [(Array(Frame,-1), "frames"); (Int, "fps"); (Int, "height"); (Int, "width") ])]
  in

  (* Add function name to symbol table *)
  let add_func map fd =
    let built_in_err = "function " ^ fd.fname ^ " may not be defined"
    and dup_err = "duplicate function " ^ fd.fname
    and make_err er = raise (Failure er)
    and n = fd.fname (* Name of the function *)
    in match fd with (* No duplicate functions or redefinitions of built-ins *)
         _ when StringMap.mem n built_in_decls -> make_err built_in_err
       | _ when StringMap.mem n map -> make_err dup_err
       | _ ->  StringMap.add n fd map
  in

  (* Collect all other function names into one symbol table *)
  let function_decls = List.fold_left add_func built_in_decls functions
  in

  (* Return a function from our symbol table *)
  let find_func s =
    try StringMap.find s function_decls
    with Not_found -> raise (Failure ("unrecognized function " ^ s))
  in

  let _ = find_func "main" in (* Ensure "main" is defined *)

    (* Raise an exception if the given rvalue type cannot be assigned to
       the given lvalue type *)
    let check_assign lvaluet rvaluet err = match lvaluet with
        Pix | Placement | Frame | Struct(_) ->
            if lvaluet = rvaluet || rvaluet = Null then lvaluet else raise (Failure err)
       | Array(t, _) -> (match rvaluet with
           Array(t, _) -> lvaluet
         | _ -> raise(Failure (err)))
       | _ -> if lvaluet = rvaluet then lvaluet else raise (Failure err)
    in

    (* Build local symbol table of variables for this function *)
    let symbols = List.fold_left (fun m (ty, name) -> StringMap.add name ty m)
	                StringMap.empty (get_binds globals')
    in

    (* Return a variable from our local symbol table *)
    let type_of_identifier s map =
      try StringMap.find s map
      with Not_found -> raise (Failure ("undeclared identifier " ^ s))
    in

    (* Return a semantically-checked expression, i.e., with a type *)
    let rec check_expr e map = match e with
        Literal   l -> (map, Int, SLiteral l)
      | Fliteral  l -> (map, Float, SFliteral l)
      | BoolLit   l -> (map, Bool, SBoolLit l)
      | StringLit l -> (map, String, SStringLit l)
      | Noexpr      -> (map, Void, SNoexpr)
      | NullLit     -> (map, Null, SNullLit)
      | Id        s -> (map, type_of_identifier s map, SId s)
      | Assign(le, e) as ex ->
          let (map, rt, e') = check_expr e map in
          let (map, t_le, le') = check_expr le map
          in let err = "illegal assignment " ^ string_of_typ t_le ^ " = " ^
            string_of_typ rt ^ " in " ^ string_of_expr ex
          in let type_check = match le' with
              SId(s) -> (map, check_assign t_le rt err, SAssign((map, t_le, le'), (map, rt, e')))
            | SAccessArray(id, idx) ->
                    (map, check_assign t_le rt err, SAssign((map, t_le, le'), (map, rt, e')))
            | _ -> raise (Failure(err)) in type_check
      | Unop(op, e) as ex ->
          let (map, t, e') = check_expr e map in
          let ty = match op with
            Neg when t = Int || t = Float -> t
          | Not when t = Bool -> Bool
          | PreIncrement | PreDecrement when t = Int -> Int
          | _ -> raise (Failure ("illegal unary operator " ^
                                 string_of_uop op ^ string_of_typ t ^
                                 " in " ^ string_of_expr ex))
          in (map, ty, SUnop(op, (map, t, e')))
      | PostUnop(e, op) as ex ->
          let (map, t, e') = check_expr e map in
          let ty = match op with
            PostIncrement when t = Int -> t
          | PostDecrement when t = Int -> t
          | _ -> raise (Failure ("illegal postfix unary operator " ^
                                 string_of_typ t ^ " in " ^ string_of_expr ex ^
                                 string_of_post_uop op))
          in (map, ty, SPostUnop((map, t, e'), op))
      | Binop(e1, op, e2) as e ->
          (* Determine expression type based on operator and operand types *)
          let (_, t1, e1') = check_expr e1 map
          and (_, t2, e2') = check_expr e2 map in
          let same = t1 = t2 in
          let ty = match op with
            Add when (same && t1 = String) || ((t1 = Int || t1 = Float) &&
                (t2 = Int || t2 = Float)) -> (match same with
                      true -> t1
                    | false -> Float)
          | Sub | Mult | Div when (t1 = Float || t1 = Int) &&
                (t2 = Float || t2 = Int) -> (match same with
                      true -> t1
                    | false -> Float)
          | Mod when same && t1 = Int -> Int
          | Equal | Neq when same -> Bool
          | Less | Leq | Greater | Geq
                     when same && (t1 = Int || t1 = Float) -> Bool
          | And | Or when same && t1 = Bool -> Bool
          | _ -> raise (
	      Failure ("illegal binary operator " ^
                       string_of_typ t1 ^ " " ^ string_of_op op ^ " " ^
                       string_of_typ t2 ^ " in " ^ string_of_expr e))
          in (map, ty, SBinop((map, t1, e1'), op, (map, t2, e2')))
      | Call(fname, args) as call -> (match fname with
          "print" -> if List.length args != 1 then
              raise (Failure ("expecting 1 argument in print"))
           else let (m, et, e') = check_expr (List.hd args) map in (match et with
               Int -> (map, Void, SCall("printi", [m, et, e']))
             | Float -> (map, Void, SCall("printf", [m, et, e']))
             | String -> (map, Void, SCall("prints", [m, et, e']))
             | Bool -> (map, Void, SCall("printb", [m, et, e']))
             | _ -> raise (Failure ("invalid argument for print")))
         | _ ->
          let fd = find_func fname in
          let param_length = List.length fd.formals in
          if List.length args != param_length then
            raise (Failure ("expecting " ^ string_of_int param_length ^
                            " arguments in " ^ string_of_expr call))
          else let check_call (ft, _) e =
            let (map, et, e') = check_expr e map in
            let err = "illegal argument found " ^ string_of_typ et ^
                      " expected " ^ string_of_typ ft ^ " in " ^ string_of_expr e
            in (map, check_assign et ft err, e')
          in
          let args' = List.map2 check_call fd.formals args
          in (map, fd.typ, SCall(fname, args')))
      | New(t, args) as new_l ->
          let len_err len = "expecting " ^ string_of_int len ^
                            " arguments in " ^ string_of_expr new_l
          and check_arg ft e =
            let (map, et, e') = check_expr e map in
            let err = "illegal argument found " ^ string_of_typ et ^
                      " expected " ^ string_of_typ ft ^ " in " ^ string_of_expr e
            in (map, check_assign ft et err, e')
          in let new_obj = match t with
              Pix       -> let check_pix args =
                  if List.length args != 0 then raise (Failure (len_err 0)) else []
                in (map, Pix, SNew(Pix, check_pix args))
            | Placement -> let check_placement args =
                  if List.length args != 5 then raise (Failure (len_err 5)) else
                      List.map2 check_arg [Pix; Int; Int; Int; Int] args
                in (map, Placement, SNew(Placement, check_placement args))
            | Frame     -> let check_frame args =
                  if List.length args != 0 then raise (Failure (len_err 0)) else
                    List.map2 check_arg [] args
                in (map, Frame, SNew(Frame, check_frame args))
            | _           -> raise (Failure ("illegal object name " ^
                        string_of_typ t))
          in new_obj
      | NewArray(s, size) as e ->
          let arr = match s with
              Int       -> (map, Array(Int, size), SNewArray(Int, size))
            | Float     -> (map, Array(Float, size), SNewArray(Float, size))
            | Bool      -> (map, Array(Bool, size), SNewArray(Bool, size))
            | String    -> (map, Array(String, size), SNewArray(String, size))
            | Pix       -> (map, Array(Pix, size), SNewArray(Pix, size))
            | Placement -> (map, Array(Placement, size), SNewArray(Placement, size))
            | Frame     -> (map, Array(Frame, size), SNewArray(Frame, size))
            | _           -> raise (Failure ("illegal array type in " ^
                                string_of_expr e))
        in if size > -1 then arr else
            raise (Failure ("illegal array size in " ^ string_of_expr e))
      | CreateArray(args) as e ->
          let err ext et = "illegal expression found " ^ string_of_typ et ^
                "expected " ^ string_of_typ ext ^ " in " ^ string_of_expr e
          in let check_types sexprs expr =
            let (map, et, e') = check_expr expr map in
              match sexprs with
                (x, y, z) :: tl -> if (y) = et then (map, et, e') :: sexprs else raise
                        (Failure (err (y) et))
                | _ -> (map, et, e') :: sexprs
          in let result = List.fold_left check_types [] args in
          let arr = match result with
              (x,y,z) :: tl -> (map, Array(y, (List.length args)), SCreateArray(result))
            | _ -> (map, Array(Notyp, 0), SCreateArray(result))
          in arr
      | AccessArray(id, e2) ->
          let typ_err = id ^ " is not an array"
          in let (map2, et2, e2') = check_expr e2 map
          in let check_access = match (type_of_identifier id map) with
              (Array(typ, _)) -> (map2, typ, SAccessArray(id, (map2, et2, e2')))
            | _            -> raise (Failure (typ_err))
          in check_access
      | SubArray(id, beg, end') ->
        let typ_err = id ^ " is not an array"
        in let check_access =
          match (type_of_identifier id map) with
            (Array(typ, _)) -> 0
          | _            -> raise (Failure (typ_err))
           in
           if beg <= end' then let _ = check_access in (map, Notyp,SSubArray(id, beg, end'))
            else raise(Failure("begin index must be less than end index for array " ^ id))
      | AccessStruct(s, field) -> if true then raise (Failure ("AccessStructure not yet implemented")) else (map, Notyp, SSubArray("", 0, 0))
    in

    (* Return a semantically-checked statement i.e. containing sexprs *)
    let rec check_stmt e map func loop_count = match e with
        Expr e -> (map, SExpr (check_expr e map))
      | If(e, s1, s2, s3) -> 
          (map, SIf(check_expr e map, check_stmt s1 map func loop_count,
          check_stmt s2 map func loop_count, check_stmt s3 map func loop_count))
      | ElseIf(e, s) -> (map, SElseIf(check_expr e map, check_stmt s map func loop_count))
      | CreateStruct(x,y) -> raise (Failure("CreateStruct not yet implemented"))
      | For(e1, e2, e3, st) -> let (x,y,z) = check_expr e1 map in
                               let (x', y',z') = check_expr e2 x in
                               let (x'',y'',z'') = check_expr e3 x' in
      	  (x'', SFor((x,y,z), (x',y',z'), (x'',y'',z''), check_stmt st x'' func
          (loop_count + 1)))
      | While(p, s) -> (map, SWhile(check_expr p map, check_stmt s map func
          (loop_count + 1)))
      | Return e -> let (map, t, e') = check_expr e map in
        if t = func.typ then (map, SReturn (map, t, e'))
        else raise (
          Failure ("return gives " ^ string_of_typ t ^ " expected " ^
                   string_of_typ func.typ ^ " in " ^ string_of_expr e))

	    (* A block is correct if each statement is correct and nothing
	       follows any Return statement.  Nested blocks are flattened. *)
      | Block sl ->
          let rec check_stmt_list e map = match e with
              [Return _ as s] -> [check_stmt s map func loop_count]
            | Return _ :: _   -> raise (Failure "nothing may follow a return")
            | Block sl :: ss  -> check_stmt_list (sl @ ss) map (* Flatten blocks *)
            | s :: ss         -> let (x,y) = check_stmt s map func loop_count
                 in (x,y) ::check_stmt_list ss x
            | []              -> []
          in (map, SBlock(check_stmt_list sl map))
      | ObjCall(e, func, args) as s ->
          let err = "illegal object function call " ^ string_of_stmt s in
          let check_func func_params args = if List.length args !=
              List.length func_params then raise (Failure(err)) else
                let check_call (ft, _) e =
                    let (map, et, e') = check_expr e map
                in (map, check_assign ft et err, e')
             in List.map2 check_call func_params args
          in let checked_expr = check_expr e map
          in let check_rgb arr = let rgb = check_expr
            (List.hd arr) map in (match rgb with
                (_,Array(Int,3),_) -> rgb
              | _ -> raise(Failure("Invalid rgb input")))
          in let check_it = match checked_expr with
              (_, Pix, _) -> (match func with
                  "makeRectangle" | "makeEllipse" -> let _ = check_rgb (List.rev args) in
                    (map, SObjCall(checked_expr, func, check_func 
                        [(Int, "width"); (Int, "height");(Array(Int, 3),"rgb")]
                        args))
                 | "makeTriangle" -> let _ = check_rgb (List.rev args) in
                    (map, SObjCall(checked_expr, func, check_func 
                        [(Int, "length");(Array(Int, 3),"rgb")]
                        args))
                 | _ -> raise(Failure("ObjCall not yet implemented")))
            | (_, Frame, _) -> if func <> "addPlacement" then raise (Failure(err)) else
                  (map, SObjCall(checked_expr, func, check_func [(Placement, "place")] args))
            | _ -> raise (Failure(err))
          in check_it
      | VarDecs(field) -> let t = fst (fst (get_first field)) in 
         let vardecs (map, vardecs_list) (b, e) = 
         let s = snd b
         in let _ = (if t=Void then raise(Failure("Void type declaration")))
         in (if StringMap.mem s map then raise ( Failure ("Duplicate variable declaration " ^ s))
             else let (map, t2, _) = check_expr e map in
                  let new_symbols = StringMap.add s t2 map
                     in let err = "LHS type of " ^ string_of_typ t ^ " not the same as " ^
                       "RHS type of " ^ string_of_typ t2 in
                     let _ = if t2 <> Void then check_assign t t2 err else t in
             (new_symbols, (b, check_expr e new_symbols) :: vardecs_list)) in
         let (new_symbols, vardecs_list) = List.fold_left vardecs (map, []) field in 
            (new_symbols, SVarDecs(List.rev vardecs_list))

      | Continue -> if loop_count > 0 then (map, SContinue) else
          raise(Failure("Continue statement not in loop"))
      | Break -> if loop_count > 0 then (map, SBreak) else
          raise(Failure("Break statement not in loop"))
      | _ -> raise (Failure("To implement statement"))

      in let check_function func =
    let formals' = check_binds "formal" func.formals in
    let sbody_stmt = check_stmt (Block func.body) symbols func 0

    in (* body of check_function *)
    { styp = func.typ;
      sfname = func.fname;
      sformals = formals';
      slocals = StringMap.fold map_to_svar (fst sbody_stmt) [];
      sbody = match sbody_stmt with
	(_ , SBlock(sl)) -> sl
      | _ -> let err = "internal error: block didn't become a block?"
      in raise (Failure err)
    }
    
    in let global_var_check svar_list var_list =
      let t = fst (fst (get_first var_list)) in
      let vardecs vardecs_list ((_,s), e) =
        let (map2,t2,e2) = check_expr e StringMap.empty
        in let err = "LHS type of " ^ string_of_typ t ^ " not the same as " ^
        "RHS type of " ^ string_of_typ t2 in
        let _ = (if t2 <> Void then check_assign t t2 err else t)
        in ((t,s), (map2,t2,e2)) :: vardecs_list
      in let vardecs_list = List.rev (List.fold_left vardecs [] var_list)
      in vardecs_list :: svar_list
    
    in let globals' = List.fold_left global_var_check [] globals'
    in (globals', List.map check_function functions)
