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
  in

  (* Check vars to see if duplicate or void type -- new thing someone check pls *)
  let check_vars (kind : string) (to_check: var list) =
    let to_check = List.map snd to_check
    in let _ = List.fold_left check_it [] (List.sort compare to_check)
       in to_check
  in


  (**** Checking Global Variables ****)

  let globals' = check_binds "global" globals in


  (* Collect function declarations for built-in functions: no bodies *)
  let built_in_decls =
    let add_bind map (name, ty) = StringMap.add name {
      typ = Void; fname = name;
      formals = [(ty, "x")];
      locals = []; body = [] } map
    in List.fold_left add_bind StringMap.empty [ ("print", Void);
                                                 ("render", Void);
                                                 ("getHash", Int) ]
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

  let check prog =
      let objects = prog.o
      and functions = prog.f
      and vars = prog.v
  in

  let check_function func =
    (* Make sure no formals or locals are void or duplicates *)
    let formals' = check_binds "formal" func.formals in
    let locals' = check_vars "local" func.locals in


    (* Raise an exception if the given rvalue type cannot be assigned to
       the given lvalue type *)
    let check_assign lvaluet rvaluet err =
       if lvaluet = rvaluet then lvaluet else raise (Failure err)
    in

    (* Build local symbol table of variables for this function *)
    let symbols = List.fold_left (fun m (ty, name) -> StringMap.add name ty m)
	                StringMap.empty (globals' @ formals' @ (List.map snd locals') )
    in

    (* Return a variable from our local symbol table *)
    let type_of_identifier s =
      try StringMap.find s symbols
      with Not_found -> raise (Failure ("undeclared identifier " ^ s))
    in

    (* Return a semantically-checked expression, i.e., with a type *)
    let rec check_expr = function
        Literal   l -> (Int, SLiteral l)
      | Fliteral  l -> (Float, SFliteral l)
      | BoolLit   l -> (Bool, SBoolLit l)
      | StringLit l -> (String, SStringLit l)
      | Noexpr    l -> (Void, SNoexpr)
      | Null      l -> (Null, SNull)
      | Id        l -> (type_of_identifier l, SId l)
      | Assign(var, e) as ex ->
          let lt = type_of_identifier var
          and (rt, e') = check_expr e in
          let err = "illegal assignment " ^ string_of_typ lt ^ " = " ^
            string_of_typ rt ^ " in " ^ string_of_expr ex
          in (check_assign lt rt err, SAssign(var, (rt, e')))
      | Unop(op, e) as ex ->
          let (t, e') = check_expr e in
          let ty = match op with
            Neg when t = Int || t = Float -> t
          | Not when t = Bool -> Bool
          | _ -> raise (Failure ("illegal unary operator " ^
                                 string_of_uop op ^ string_of_typ t ^
                                 " in " ^ string_of_expr ex))
          in (ty, SUnop(op, (t, e')))
      | Binop(e1, op, e2) as e ->
          let (t1, e1') = check_expr e1
          and (t2, e2') = check_expr e2 in
          (* All binary operators require operands of the same type *)
          let same = t1 = t2 in
          (* Determine expression type based on operator and operand types *)
          let ty = match op with
            Add | Sub | Mult | Div when same && t1 = Int   -> Int
          | Add | Sub | Mult | Div when same && t1 = Float -> Float
          | Equal | Neq            when same               -> Bool
          | Less | Leq | Greater | Geq
                     when same && (t1 = Int || t1 = Float) -> Bool
          | And | Or when same && t1 = Bool -> Bool
          | _ -> raise (
	      Failure ("illegal binary operator " ^
                       string_of_typ t1 ^ " " ^ string_of_op op ^ " " ^
                       string_of_typ t2 ^ " in " ^ string_of_expr e))
          in (ty, SBinop((t1, e1'), op, (t2, e2')))
      | Call(fname, args) as call ->
          let fd = find_func fname in
          let param_length = List.length fd.formals in
          if List.length args != param_length then
            raise (Failure ("expecting " ^ string_of_int param_length ^
                            " arguments in " ^ string_of_expr call))
          else let check_call (ft, _) e =
            let (et, e') = check_expr e in
            let err = "illegal argument found " ^ string_of_typ et ^
                      " expected " ^ string_of_typ ft ^ " in " ^ string_of_expr e
            in (check_assign ft et err, e')
          in
          let args' = List.map2 check_call fd.formals args
          in (fd.typ, SCall(fname, args'))
      | New(t, args) as new_l ->
          let len_err len = "expecting " ^ string_of_int len ^
                            " arguments in " ^ string_of_expr new_l
          and typ_err index t1 t2  = "expected arg " ^ string_of_int index ^ " of " ^
                                     string_of_expr new_l ^ "to be of type " ^ t1 ^ ", got " ^
                                     string_of_typ t2 ^ "instead"
          and check_pix num =
            if List.length num != 0 then raise (Failure (len_err 0)) else num
          and check_placement num2 =
            if List.length num2 != 5 then raise (Failure (len_err 5)) else num2
              (* if EACH PARAM IS NOT CORRECT TYPE then raise (Failure typ_err index t1 t2) *)
          and check_frame num3 =
            if List.length num3 != 2 then raise (Failure (len_err 2)) else num3
              (* check param types *)
          in let _ = match t with
              Pix       -> (t, SNew(t, check_pix args))
            | Placement -> (t, SNew(t, check_placement args))
            | Frame     -> (t, SNew(t, check_frame args))
      | NewArray(t, size) as e ->
          if size = Int then (t, SNewArray(t, size))
          else raise (Failure ("expected type Int as Array size"))
      | CreateArray(args) as e ->
          if args = Void then (Void, SCreateArray(Void))
          else let rec check_args arguments=
            match arguments with
            |[n] -> if check_expr n then (type_of_identifier n, SCreateArray(type_of_identifier n))
            |hd::tl -> if check_expr hd then check_args tl
            in check_args args
(* Still needs to be implemented *)
      | AccessArray(s, ind) as e ->

(* not needed
      | SubArray(s, beg, end') as e ->
      | AccessStruct(s, field) as e ->
      | PostIncrement(e1) as e ->
      | PostDecrement(e1) as e ->
*)

    in

    let check_bool_expr e =
      let (t', e') = expr e
      and err = "expected Boolean expression in " ^ string_of_expr e
      in if t' != Bool then raise (Failure err) else (t', e')
    in

    (* Return a semantically-checked statement i.e. containing sexprs
       DOUBLE CHECK THIS *)
    let rec check_stmt = function
        Expr e -> SExpr (expr e)
      | If(p, b1, b2) -> SIf(check_bool_expr p, check_stmt b1, check_stmt b2)
      | For(e1, e2, e3, st) ->
	  SFor(expr e1, check_bool_expr e2, expr e3, check_stmt st)
      | While(p, s) -> SWhile(check_bool_expr p, check_stmt s)
      | Return e -> let (t, e') = expr e in
        if t = func.typ then SReturn (t, e')
        else raise (
	  Failure ("return gives " ^ string_of_typ t ^ " expected " ^
		   string_of_typ func.typ ^ " in " ^ string_of_expr e))

	    (* A block is correct if each statement is correct and nothing
	       follows any Return statement.  Nested blocks are flattened. *)
      | Block sl ->
          let rec check_stmt_list = function
              [Return _ as s] -> [check_stmt s]
            | Return _ :: _   -> raise (Failure "nothing may follow a return")
            | Block sl :: ss  -> check_stmt_list (sl @ ss) (* Flatten blocks *)
            | s :: ss         -> check_stmt s :: check_stmt_list ss
            | []              -> []
          in SBlock(check_stmt_list sl)

    in
    let check_bool_expr e =
      let (t', e') = expr e
      and err = "expected Boolean expression in " ^ string_of_expr e
      in if t' != Bool then raise (Failure err) else (t', e')
    in


    (* Return a semantically-checked statement i.e. containing sexprs *)
    let rec check_stmt = function
        Expr e -> SExpr (check_expr e)
      | If(p, b1, b2) -> SIf(check_bool_expr p, check_stmt b1, check_stmt b2)
      | For(e1, e2, e3, st) ->
	  SFor(expr e1, check_bool_expr e2, expr e3, check_stmt st)
      | While(p, s) -> SWhile(check_bool_expr p, check_stmt s)
      | Return e -> let (t, e') = check_expr e in
        if t = func.typ then SReturn (t, e')
        else raise (
          Failure ("return gives " ^ string_of_typ t ^ " expected " ^
                   string_of_typ func.typ ^ " in " ^ string_of_expr e))

	    (* A block is correct if each statement is correct and nothing
	       follows any Return statement.  Nested blocks are flattened. *)
      | Block sl ->
          let rec check_stmt_list = function
              [Return _ as s] -> [check_stmt s]
            | Return _ :: _   -> raise (Failure "nothing may follow a return")
            | Block sl :: ss  -> check_stmt_list (sl @ ss) (* Flatten blocks *)
            | s :: ss         -> check_stmt s :: check_stmt_list ss
            | []              -> []
          in SBlock(check_stmt_list sl)
      | VarDec var -> let _ = check_expr snd var in SVarDec(var)
      | ObjCall(name, func, args) ->
          let meth_err t = "Method " ^ string_of_fdecl fdecl ^ " does not exist " ^
                           " in " ^ string_of_typ t
          and t = type_of_identifier name
          and f = if StringMap.find func (StringMap.find name symbols).methods then
                  f else raise (Failure (meth_err t))
          and formals_len = List.length f.formals
          and _ = if formals_len != List.length args then
                  raise (Failure (len_err formals_len)) else args
          in (t, SObjCall(name, func, args))

(*
      | CreateStruct(s, var_l_l) ->
      | VarDecs(var_l)
      | Continue l ->
      | Break l ->
*)

    in (* body of check_function *)
    { styp = func.typ;
      sfname = func.fname;
      sformals = formals';
      slocals  = locals';
      sbody = match check_stmt (Block func.body) with
	SBlock(sl) -> sl
      | _ -> let err = "internal error: block didn't become a block?"
      in raise (Failure err)
    }
in (globals', List.map check_function functions)
