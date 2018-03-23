type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq |
          And | Or | Mod

type uop = Neg | Not

type typ = Int | Bool | Float | String | Void | Pix | Placement | Frame | Notyp |
           Array of typ | Struct of string

type null = Null

type expr =
    Literal of int
  | Fliteral of float
  | BoolLit of bool
  | StringLit of string
  | Id of string
  | Binop of expr * op * expr
  | Unop of uop * expr
  | Assign of string * expr
  | Call of string * expr list
  | Noexpr
  | Null
  | New of typ * expr list
  | NewArray of typ * int
  | CreateArray of expr list
  | SubArray of string * int * int
  | AccessArray of string * int
  | AccessStruct of string * string
  | PostIncrement of expr
  | PostDecrement of expr

type bind = typ * string

type var = bind * expr

type stmt =
    Block of stmt list
  | Expr of expr
  | Return of expr
  | If of expr * stmt * stmt list * stmt
  | ElseIf of expr * stmt
  | For of expr * expr * expr * stmt
  | While of expr * stmt
  | Break
  | Continue
  | VarDecs of var list
  | VarDec of var
  | ObjCall of string * string * expr list
  | CreateStruct of string * var list list

type func_decl = {
    typ : typ;
    fname : string;
    formals : bind list;
    locals : var list;
    body : stmt list;
  }

type obj = {
  typ: typ; (*Pix, Frame, Placement*)
  oname: string;
  ctor: fdecl;
  decls: bind list;
  methods: func_decl list;
}

type program = var list list * func_decl list

(* Pretty-printing functions *)

let string_of_op = function
    Add -> "+"
  | Sub -> "-"
  | Mult -> "*"
  | Div -> "/"
  | Equal -> "=="
  | Neq -> "!="
  | Less -> "<"
  | Leq -> "<="
  | Greater -> ">"
  | Geq -> ">="
  | And -> "&&"
  | Or -> "||"
  | Mod -> "%"

let string_of_uop = function
    Neg -> "-"
  | Not -> "!"

let rec string_of_typ = function
    Int -> "Int"
  | Bool -> "Boolean"
  | Float -> "Float"
  | String -> "String"
  | Void -> "Void"
  | Pix -> "Pix"
  | Placement -> "Placement"
  | Frame -> "Frame"
  | Notyp -> ""
  | Array(t) -> string_of_typ t ^ "[]"
  | Struct(s) -> "Struct " ^ s

let rec string_of_expr = function
    Literal(l) -> string_of_int l
  | Fliteral(l) -> string_of_float l
  | BoolLit(true) -> "true"
  | BoolLit(false) -> "false"
  | StringLit(l) -> l
  | Id(s) -> s
  | Binop(e1, o, e2) ->
      string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Unop(o, e) -> string_of_uop o ^ string_of_expr e
  | Assign(v, e) -> v ^ " = " ^ string_of_expr e
  | Call(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | Noexpr -> ""
  | Null -> "null"
  | New(t, el) ->
     "new " ^ string_of_typ t ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | CreateArray(el) -> "[" ^ String.concat "," (List.map string_of_expr el) ^ "]"
  | SubArray(id, i1, i2) -> id ^ "[" ^ string_of_int i1 ^ ":" ^ string_of_int i2 ^ "]"
  | AccessArray(id, i) -> id ^ "[" ^ string_of_int i ^ "]"
  | NewArray(t, i) -> "new " ^ string_of_typ t ^ "[" ^ string_of_int i ^ "]"
  | AccessStruct(i1, i2) -> i1 ^ "." ^ i2
  | PostIncrement(e) -> "(" ^ string_of_expr e ^ ")++"
  | PostDecrement(e) -> "(" ^ string_of_expr e ^ ")--"

let string_of_vdecl ((t, id), value) =
  match value with
  | Noexpr -> string_of_typ t ^ " " ^ id
  | _ -> string_of_typ t ^ " " ^ id ^ " = " ^ string_of_expr value

let string_of_vdecls (vars) = String.concat "," (List.map string_of_vdecl vars) ^
    if (List.length vars) > 0 then ";\n" else ""

let rec string_of_stmt = function
    Block(stmts) ->
      "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n";
  | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n";
  | If(e, s, stmts, Block([])) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s
      ^ String.concat "" (List.map string_of_stmt stmts)
  | If(e, s1, stmts, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
      string_of_stmt s1 ^ String.concat "" (List.map string_of_stmt stmts) ^ "else\n" ^ string_of_stmt s2
  | ElseIf(e, s) -> "else if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s
  | For(e1, e2, e3, s) ->
      "for (" ^ string_of_expr e1  ^ " ; " ^ string_of_expr e2 ^ " ; " ^
      string_of_expr e3  ^ ") " ^ string_of_stmt s
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s
  | Break -> "break;\n"
  | Continue -> "continue;\n"
  | VarDecs(vars) -> String.concat "," (List.map string_of_vdecl vars) ^ ";\n"
  | ObjCall(o, f, el) -> o ^ "." ^ f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ");\n"
  | CreateStruct(s, vdecls) -> "Struct " ^ s ^ "\n{\n" ^
      String.concat "" (List.map string_of_vdecls vdecls) ^ "};\n"

let string_of_fdecl fdecl =
  string_of_typ fdecl.typ ^ " " ^
  fdecl.fname ^ "(" ^ String.concat ", " (List.map snd fdecl.formals) ^
  ")\n{\n" ^
  String.concat "" (List.map string_of_stmt fdecl.body) ^
  "}\n"

let string_of_program (vars_list, funcs) =
  String.concat "" (List.map string_of_vdecls (List.rev vars_list)) ^ "\n" ^
  String.concat "\n" (List.map string_of_fdecl (List.rev funcs))
