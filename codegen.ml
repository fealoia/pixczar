module L = Llvm
module A = Ast
open Sast 

module StringMap = Map.Make(String)

(* Code Generation from the SAST. Returns an LLVM module if successful,
   throws an exception if something is wrong. *)
let translate (_, functions) =
  let context    = L.global_context () in
  (* Add types to the context so we can use them in our LLVM code *)
  let i32_t      = L.i32_type    context
  and i8_t       = L.i8_type     context 
  and void_t     = L.void_type   context 
  and str_t      = L.pointer_type (L.i8_type context)
  and float_t    = L.double_type context
  and i1_t       = L.i1_type     context
  (* Create an LLVM module -- this is a "container" into which we'll 
     generate actual code *)
  and the_module = L.create_module context "MicroC" in

  (* Convert MicroC types to LLVM types *)
  let ltype_of_typ = function
      A.Int    -> i32_t
    | A.Void   -> void_t
    | A.String -> str_t
    | A.Float  -> float_t
    | A.Bool   -> i1_t
  (*  | A.Pix -> 
    | A.Placement ->
    | A.Frame ->*)
    | t -> raise (Failure ("Type " ^ A.string_of_typ t ^ " not implemented yet"))
  in

  let global_var m (t, n) =
      let init = match t with
      A.Float -> L.const_float (ltype_of_typ t) 0.0
    | _ -> L.const_int (ltype_of_typ t) 0
      in StringMap.add n (L.define_global n init the_module) m in

  (* declare i32 @printf(i8*, ...) *)
  let printf_t : L.lltype = 
      L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let printf_func : L.llvalue = 
     L.declare_function "printf" printf_t the_module in 

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
  
  (* Fill in the body of the given function *)
  let build_function_body fdecl =
    let (the_function, _) = StringMap.find fdecl.sfname function_decls in
    let builder = L.builder_at_end context (L.entry_block the_function) in

    let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder
    and float_format_str = L.build_global_stringptr "%g\n" "fmt" builder in

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
      let add_local m (t, n) =
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
      | SFliteral l -> L.const_float float_t l
      | SBoolLit b -> L.const_int i1_t (if b then 1 else 0)
      | SNoexpr -> L.const_int i32_t 0
      | SId s -> L.build_load (lookup s) s builder 
      | SAssign (s, e) -> let e' = expr builder e in
          let _ = L.build_store e' (lookup s) builder 
          in e'
      | SCall ("print", [e]) ->
	  L.build_call printf_func [| int_format_str ; (expr builder e) |]
	    "printf" builder 
      (* Throw an exception for any other expressions *)
      | _ -> to_imp (string_of_sexpr (A.Int,e))  
    in

    let rec stmt builder = function
        SExpr e -> let _ = expr builder e in builder 
      | SBlock sl -> List.fold_left stmt builder sl
      (* return 0;  ----->  ret i32 0 *)
      | SReturn e -> let _ = match fdecl.styp with
                              A.Int -> L.build_ret (expr builder e) builder 
                            | _ -> to_imp (A.string_of_typ fdecl.styp)
                     in builder
      | s -> to_imp (string_of_sstmt s)

    in ignore (stmt builder (SBlock fdecl.sbody))
  (* Build each function (there should only be one for Hello World), 
     and return the final module *)
  in List.iter build_function functions; the_module
