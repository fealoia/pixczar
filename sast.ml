open Ast

type sexpr = typ * sx
and sx =
    SLiteral of int
  | SFliteral of float
  | SBoolLit of bool
  | SStringLit of string
  | SId of string
  | SBinop of sexpr * op * sexpr
  | SUnop of uop * sexpr
  | SAssign of string * sexpr
  | SCall of string * sexpr list
  | SNoexpr
  | SNull
  | SNew of typ * sexpr list
  | SNewArray of typ * int
  | SCreateArray of sexpr list
  | SSubArray of string * int * int
  | SAccessArray of string * int
  | SAccessStruct of string * string
  | SPostIncrement of sexpr
  | SPostDecrement of sexpr

type svar = bind * sexpr

type sstmt =
    SBlock of sstmt list
  | SExpr of sexpr
  | SReturn of sexpr
  | SIf of sexpr * sstmt * sstmt list * sstmt
  | SElseIf of sexpr * sstmt
  | SFor of sexpr * sexpr * sexpr * sstmt
  | SWhile of sexpr * sstmt
  | SBreak
  | SContinue
  | SVarDecs of svar list
  | SObjCall of string * string * sexpr list
  | SCreateStruct of string * svar list list

type sfunc_decl = {
    styp : typ;
    sfname : string;
    sformals : bind list;
    slocals : svar list;
    sbody : sstmt list;
  }

type sprogram = svar list list * sfunc_decl list

(* Pretty-printing functions *)

let rec string_of_sexpr = function
    SLiteral(l) -> string_of_int l
  | SFliteral(l) -> string_of_float l
  | SBoolLit(true) -> "true"
  | SBoolLit(false) -> "false"
  | SStringLit(l) -> l
  | SId(s) -> s
  | SBinop(e1, o, e2) ->
      string_of_sexpr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_sexpr e2
  | SUnop(o, e) -> string_of_uop o ^ string_of_sexpr e
  | SAssign(v, e) -> v ^ " = " ^ string_of_sexpr e
  | SCall(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_sexpr el) ^ ")"
  | SNoexpr -> ""
  | SNull -> "null"
  | SNew(t, el) ->
     "new " ^ string_of_typ t ^ "(" ^ String.concat ", " (List.map string_of_sexpr el) ^ ")"
  | SCreateArray(el) -> "[" ^ String.concat "," (List.map string_of_sexpr el) ^ "]"
  | SSubArray(id, i1, i2) -> id ^ "[" ^ string_of_int i1 ^ ":" ^ string_of_int i2 ^ "]"
  | SAccessArray(id, i) -> id ^ "[" ^ string_of_int i ^ "]"
  | SNewArray(t, i) -> "new " ^ string_of_typ t ^ "[" ^ string_of_int i ^ "]"
  | SAccessStruct(i1, i2) -> i1 ^ "." ^ i2
  | SPostIncrement(e) -> "(" ^ string_of_sexpr e ^ ")++"
  | SPostDecrement(e) -> "(" ^ string_of_sexpr e ^ ")--"

let string_of_svdecl ((t, id), value) =
  match value with
  | SNoexpr -> string_of_typ t ^ " " ^ id
  | _ -> string_of_typ t ^ " " ^ id ^ " = " ^ string_of_sexpr value

let string_of_svdecls (vars) = String.concat "," (List.map string_of_svdecl vars) ^
    if (List.length vars) > 0 then ";\n" else ""

let rec string_of_sstmt = function
    Block(stmts) ->
      "{\n" ^ String.concat "" (List.map string_of_sstmt stmts) ^ "}\n"
  | Expr(expr) -> string_of_sexpr expr ^ ";\n";
  | Return(expr) -> "return " ^ string_of_sexpr expr ^ ";\n";
  | If(e, s, stmts, SBlock([])) -> "if (" ^ string_of_sexpr e ^ ")\n" ^ string_of_sstmt s
      ^ String.concat "" (List.map string_of_sstmt stmts)
  | If(e, s1, stmts, s2) ->  "if (" ^ string_of_sexpr e ^ ")\n" ^
      string_of_sstmt s1 ^ String.concat "" (List.map string_of_sstmt stmts) ^ "else\n" ^ string_of_sstmt s2
  | ElseIf(e, s) -> "else if (" ^ string_of_sexpr e ^ ")\n" ^ string_of_sstmt s
  | For(e1, e2, e3, s) ->
      "for (" ^ string_of_sexpr e1  ^ " ; " ^ string_of_sexpr e2 ^ " ; " ^
      string_of_sexpr e3  ^ ") " ^ string_of_sstmt s
  | While(e, s) -> "while (" ^ string_of_sexpr e ^ ") " ^ string_of_sstmt s
  | Break -> "break;\n"
  | Continue -> "continue;\n"
  | VarDecs(vars) -> String.concat "," (List.map string_of_svdecl vars) ^ ";\n"
  | ObjCall(o, f, el) -> o ^ "." ^ f ^ "(" ^ String.concat ", " (List.map string_of_sexpr el) ^ ");\n"
  | CreateStruct(s, vdecls) -> "Struct " ^ s ^ "\n{\n" ^
      String.concat "" (List.map string_of_svdecls vdecls) ^ "};\n"

let string_of_sfdecl fdecl =
  string_of_typ fdecl.styp ^ " " ^
  fdecl.sfname ^ "(" ^ String.concat ", " (List.map snd fdecl.sformals) ^
  ")\n{\n" ^
  String.concat "" (List.map string_of_sstmt fdecl.sbody) ^
  "}\n"

let string_of_sprogram (vars_list, funcs) =
  String.concat "" (List.map string_of_svdecls (List.rev vars_list)) ^ "\n" ^
  String.concat "\n" (List.map string_of_sfdecl (List.rev funcs))
