open Ast

module StringMap = Map.Make(String)

type sexpr = typ StringMap.t * typ * sx
and sx =
    SLiteral of int
  | SFliteral of float
  | SBoolLit of bool
  | SStringLit of string
  | SId of string
  | SBinop of sexpr * op * sexpr
  | SUnop of uop * sexpr
  | SPostUnop of sexpr * post_uop
  | SAssign of sexpr * sexpr
  | SCall of string * sexpr list
  | SNoexpr
  | SNullLit
  | SNew of typ * sexpr list
  | SNewArray of typ * int
  | SCreateArray of sexpr list
  | SSubArray of string * int * int
  | SAccessArray of string * sexpr
  | SAccessStruct of string * string

type svar = bind * sexpr

type sstmt = typ StringMap.t * ss
and ss =
    SBlock of sstmt list
  | SExpr of sexpr
  | SReturn of sexpr
  | SIf of sexpr * sstmt * sstmt * sstmt
  | SElseIf of sexpr * sstmt
  | SFor of sexpr * sexpr * sexpr * sstmt
  | SWhile of sexpr * sstmt
  | SBreak
  | SContinue
  | SInclude of string
  | SVarDecs of svar list
  | SObjCall of sexpr * string * sexpr list
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

let rec string_of_sexpr (_, t, e) =
    "(" ^ string_of_typ t ^ " : " ^ (match e with
    SLiteral(l) -> string_of_int l
  | SFliteral(l) -> string_of_float l
  | SBoolLit(true) -> "true"
  | SBoolLit(false) -> "false"
  | SStringLit(l) -> "\"" ^ l ^ "\""
  | SId(s) -> s
  | SBinop(e1, o, e2) ->
      string_of_sexpr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_sexpr e2
  | SUnop(o, e) -> string_of_uop o ^ string_of_sexpr e
  | SPostUnop(e, o) -> string_of_sexpr e ^ string_of_post_uop o
  | SAssign(e1, e2) -> string_of_sexpr e1 ^ " = " ^ string_of_sexpr e2
  | SCall(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_sexpr el) ^ ")"
  | SNoexpr -> ""
  | SNullLit -> "null"
  | SNew(t, el) ->
     "new " ^ string_of_typ t ^ "(" ^ String.concat ", " (List.map string_of_sexpr el) ^ ")"
  | SCreateArray(el) -> "[" ^ String.concat "," (List.map string_of_sexpr el) ^ "]"
  | SSubArray(id, i1, i2) -> id ^ "[" ^ string_of_int i1 ^ ":" ^ string_of_int i2 ^ "]"
  | SAccessArray(id, e2) -> id ^ "[" ^ string_of_sexpr e2 ^ "]"
  | SNewArray(t, i) -> "new " ^ string_of_typ t ^ "[" ^ string_of_int i ^ "]"
  | SAccessStruct(i1, i2) -> i1 ^ "." ^ i2
    ) ^ ")"

let string_of_svdecl ((t1, id), (map, t2, value)) =
  match value with
  | SNoexpr -> string_of_typ t1 ^ " " ^ id
  | _ -> string_of_typ t1 ^ " " ^ id ^ " = " ^ string_of_sexpr (StringMap.empty, t1, value)

let string_of_svdecls (vars) = String.concat "," (List.map string_of_svdecl vars) ^
    if (List.length vars) > 0 then ";\n" else ""

let rec string_of_sstmt (map, e) = match e with
    SBlock(stmts) ->
      "{\n" ^ String.concat "" (List.map string_of_sstmt stmts) ^ "}\n"
  | SExpr(expr) -> string_of_sexpr expr ^ ";\n";
  | SReturn(expr) -> "return " ^ string_of_sexpr expr ^ ";\n";
  | SIf(e, s, stmts, (map, SBlock([]))) -> "if (" ^ string_of_sexpr e ^ ")\n" ^ string_of_sstmt s
      ^ string_of_sstmt stmts
  | SIf(e, s1, stmts, s2) ->  "if (" ^ string_of_sexpr e ^ ")\n" ^
      string_of_sstmt s1 ^ string_of_sstmt stmts ^ "else\n" ^ string_of_sstmt s2
  | SElseIf(e, s) -> "else if (" ^ string_of_sexpr e ^ ")\n" ^ string_of_sstmt s
  | SFor(e1, e2, e3, s) ->
      "for (" ^ string_of_sexpr e1  ^ " ; " ^ string_of_sexpr e2 ^ " ; " ^
      string_of_sexpr e3  ^ ") " ^ string_of_sstmt s
  | SWhile(e, s) -> "while (" ^ string_of_sexpr e ^ ") " ^ string_of_sstmt s
  | SBreak -> "break;\n"
  | SContinue -> "continue;\n"
  | SInclude(s) -> "include " ^ s ^ ";\n"
  | SVarDecs(vars) -> String.concat "," (List.map string_of_svdecl vars) ^ ";\n"
  | SObjCall(e, f, el) -> string_of_sexpr e ^ "." ^ f ^ "(" ^ String.concat ", " (List.map string_of_sexpr el) ^ ");\n"
  | SCreateStruct(s, vdecls) -> "Struct " ^ s ^ "\n{\n" ^
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
