type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq |
          And | Or | Mod

type uop = Neg | Not | PreIncrement | PreDecrement

type post_uop = PostIncrement | PostDecrement

type typ = Int | Bool | Float | String | Void | Pix | Placement | Frame | Notyp |
           Array of typ | Struct of string | Null 

type expr =
    Literal of int
  | Fliteral of float
  | BoolLit of bool
  | StringLit of string
  | Id of string
  | Binop of expr * op * expr
  | Unop of uop * expr
  | PostUnop of expr * post_uop
  | Assign of expr * expr
  | Call of string * expr list
  | Noexpr
  | NullLit 
  | New of typ * expr list
  | NewArray of typ * int
  | CreateArray of expr list
  | SubArray of string * int * int
  | AccessArray of string * expr
  | AccessStruct of expr * string

type bind = typ * string

type var = bind * expr

type stmt =
    Block of stmt list
  | Expr of expr
  | Return of expr
  | If of expr * stmt * stmt * stmt
  | ElseIf of expr * stmt 
  | For of expr * expr * expr * stmt
  | While of expr * stmt
  | Break 
  | Continue
  | Include of string
  | VarDecs of var list
  | ObjCall of expr * string * expr list
  | CreateStruct of string * var list list

type func_decl = {
    typ : typ;
    fname : string;
    formals : bind list;
    locals : var list list;
    body : stmt list;
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
  | PreIncrement -> "++"
  | PreDecrement -> "--"

let string_of_post_uop = function
    PostIncrement-> "++"
  | PostDecrement-> "--"

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
  | Null -> "Null"

let rec string_of_expr = function
    Literal(l) -> string_of_int l
  | Fliteral(l) -> string_of_float l
  | BoolLit(true) -> "true"
  | BoolLit(false) -> "false"
  | StringLit(l) -> l
  | Id(s) -> s
  | Binop(e1, o, e2) ->
      string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Unop(o, e) -> string_of_uop o ^ "(" ^ string_of_expr e ^ ")"
  | PostUnop(e, o) -> "(" ^ string_of_expr e ^ ")" ^ string_of_post_uop o
  | Assign(e1, e2) -> string_of_expr e1 ^ " = " ^ string_of_expr e2
  | Call(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | Noexpr -> ""
  | NullLit -> "null"
  | New(t, el) ->
     "new " ^ string_of_typ t ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | CreateArray(el) -> "[" ^ String.concat "," (List.map string_of_expr el) ^ "]"
  | SubArray(id, i1, i2) -> id ^ "[" ^ string_of_int i1 ^ ":" ^ string_of_int i2 ^ "]"
  | AccessArray(id, e2) -> id ^ "[" ^ string_of_expr e2 ^ "]"
  | NewArray(t, i) -> "new " ^ string_of_typ t ^ "[" ^ string_of_int i ^ "]"
  | AccessStruct(e, i) -> string_of_expr e ^ "." ^ i

let string_of_vdecl ((t, id), value) = 
  match value with
  | Noexpr -> string_of_typ t ^ " " ^ id
  | _ -> string_of_typ t ^ " " ^ id ^ " = " ^ string_of_expr value

let string_of_vdecls (vars) = String.concat "," (List.map string_of_vdecl vars) ^
    if (List.length vars) > 0 then ";\n" else ""

let rec string_of_stmt = function
    Block(stmts) ->
        if (List.length stmts) > 0 then
        "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n" else ""
  | Expr(expr) -> string_of_expr expr ^ ";\n";
  | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n";
  | If(e, s1, s2, Block([])) -> "if (" ^ string_of_expr e ^ ")\n" ^
  string_of_stmt s1 ^ string_of_stmt s2 
  | If(e, s1, s2, s3) ->  "if (" ^ string_of_expr e ^ ")\n" ^
      string_of_stmt s1 ^ string_of_stmt s2 ^ "else\n" ^ string_of_stmt s3
  | ElseIf(e, s) -> "else if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s
  | For(e1, e2, e3, s) ->
      "for (" ^ string_of_expr e1  ^ " ; " ^ string_of_expr e2 ^ " ; " ^
      string_of_expr e3  ^ ") " ^ string_of_stmt s
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s
  | Break -> "break;\n"
  | Continue -> "continue;\n"
  | Include(s) -> "include " ^ s ^";\n"
  | VarDecs(vars) -> String.concat "," (List.map string_of_vdecl vars) ^ ";\n"
  | ObjCall(e, f, el) -> string_of_expr e ^ "." ^ f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ");\n"
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
