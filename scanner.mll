(* Ocamllex scanneddr for PixCzar *)

{ open Parser }

let digit = ['0' - '9']
let digits = digit+

let stringcharacters = [' ' '\t' '\r' '\n' 'a'-'z' 'A'-'Z' '!' '"' '#' '%' '&' '\''
'(' ')' '*' '+' ',' '-' '.' '/' ':' ';' '<' '=' '>' '?' '[' '\\' ']' '^' '_' 
'{' '|' '}' '~']

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf }
| "/*"     { comment lexbuf }
| "//"     { singlelinecomment lexbuf }
| '('      { LPAREN }
| ')'      { RPAREN }
| '{'      { LBRACE }
| '}'      { RBRACE }
| ';'      { SEMI }
| ','      { COMMA }
| '+'      { PLUS }
| '-'      { MINUS }
| '*'      { TIMES }
| '/'      { DIVIDE }
| '='      { ASSIGN }
| '%'      { MOD }
| "=="     { EQ }
| "!="     { NEQ }
| '<'      { LT }
| "<="     { LEQ }
| ">"      { GT }
| ">="     { GEQ }
| "&&"     { AND }
| "||"     { OR }
| "!"      { NOT }
| "if"     { IF }
| "else"   { ELSE }
| "else if" { ELSEIF }
| "for"    { FOR }
| "while"  { WHILE }
| "break"  { BREAK }
| "continue" { CONTINUE }
| "return" { RETURN }
| "Int"    { INT }
| "Boolean"{ BOOL }
| "Float"  { FLOAT }
| "String" { STRING }
| "Void"   { VOID }
| "Pix"    { PIX }
| "Placement" { PLACEMENT }
| "Frame"  { FRAME }
| "true"   { BLIT(true)  }
| "false"  { BLIT(false) }
| "null"   { NULL }
| "new"    { NEW }
| "."    { DOT }
| digits as lxm { LITERAL(int_of_string lxm) }
| digits '.'  digit* as lxm { FLIT(float_of_string lxm) }
| ('\"' (stringcharacters* as lxm) '\"')|('\'' (stringcharacters* as lxm) '\'') { SLIT(lxm) }
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']*     as lxm { ID(lxm) }
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  "*/" { token lexbuf }
| _    { comment lexbuf }

and singlelinecomment = parse
  "\n" { token lexbuf }
| _    { singlelinecomment lexbuf }
