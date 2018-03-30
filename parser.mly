%{open Ast%}

%token SEMI LPAREN RPAREN LBRACE RBRACE COMMA PLUS MINUS TIMES DIVIDE ASSIGN MOD
%token NOT EQ NEQ LT LEQ GT GEQ AND OR
%token LBRACK RBRACK COLON
%token RETURN IF ELSEIF ELSE FOR WHILE BREAK CONTINUE
%token INT BOOL FLOAT STRING VOID PIX PLACEMENT FRAME NULL NEW DOT STRUCT
%token <int> LITERAL
%token <bool> BLIT
%token <string> ID SLIT
%token <float> FLIT
%token EOF

%start program
%type <Ast.program> program

%nonassoc NOELSE
%nonassoc ELSE
%nonassoc ELSEIF
%right ASSIGN
%left OR
%left AND
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE MOD DOT
%right NOT NEG

%%

program:
  decls EOF { $1 }

decls:
   /* nothing */ { ([], [])                                    }
 | decls vdecl_list SEMI { (((List.rev $2) :: fst $1), snd $1) }
 | decls fdecl { (fst $1, ($2 :: snd $1))                      }

fdecl:
   typ ID LPAREN formals_opt RPAREN LBRACE stmt_list RBRACE
     { { typ = $1;
	 fname = $2;
	 formals = $4;
         locals = [];
	 body = List.rev $7 } }

formals_opt:
    /* nothing */ { [] }
  | formal_list   { List.rev $1 }

formal_list:
    typ ID                   { [($1,$2)]     }
  | formal_list COMMA typ ID { ($3,$4) :: $1 }

prim_typ:
    INT    { Int    }
  | BOOL   { Bool   }
  | FLOAT  { Float  }
  | STRING { String }
  | VOID   { Void   }

nonprim_typ:
    PIX       { Pix       }
  | PLACEMENT { Placement }
  | FRAME     { Frame     }

typ:
    prim_typ          { $1         }
  | nonprim_typ       { $1         }
  | typ LBRACK RBRACK { Array($1)  }
  | STRUCT ID         { Struct($2) }

vdecl_list:
    typ vdecl              { [(($1, snd (fst $2)), snd $2)]     }
  | vdecl_list COMMA vdecl { $3 :: $1                           }

vdecl:
    ID             { ((Notyp, $1), Noexpr) }
  | ID ASSIGN expr { ((Notyp, $1), $3)     }

struct_vdecl_list:
    vdecl_list                        { [List.rev $1]       }
  | struct_vdecl_list SEMI vdecl_list { (List.rev $3) :: $1 }

stmt_list:
    /* nothing */  { [] }
  | stmt_list stmt { $2 :: $1 }

stmt:
    expr SEMI                                 { Expr $1                            }
  | RETURN expr_opt SEMI                      { Return $2                          }
  | LBRACE stmt_list RBRACE                   { Block(List.rev $2)                 }
  | IF LPAREN expr RPAREN stmt %prec NOELSE   { If($3, $5, [], Block([]))          }
  | IF LPAREN expr RPAREN stmt ELSE stmt      { If($3, $5, [], $7)                 }
  | IF LPAREN expr RPAREN stmt elseif_list %prec NOELSE
                                              { If($3, $5, List.rev $6, Block([])) }
  | IF LPAREN expr RPAREN stmt elseif_list ELSE stmt
                                              { If($3, $5, List.rev $6, $8)        }
  | FOR LPAREN expr_opt SEMI expr_opt SEMI expr_opt RPAREN stmt
                                              { For($3, $5, $7, $9)                }
  | WHILE LPAREN expr RPAREN stmt             { While($3, $5)                      }
  | BREAK SEMI                                { Break                              }
  | CONTINUE SEMI                             { Continue                           }
  | vdecl_list SEMI                           { VarDecs(List.rev $1)               }
  | expr DOT ID LPAREN args_opt RPAREN SEMI   { ObjCall($1, $3, $5)                }
  | STRUCT ID LBRACE struct_vdecl_list SEMI RBRACE SEMI
                                              { CreateStruct($2, List.rev $4)      }

elseif_list:
  | ELSEIF LPAREN expr RPAREN stmt             { [ElseIf($3, $5)]     }
  | elseif_list ELSEIF LPAREN expr RPAREN stmt { ElseIf($4, $6) :: $1 }

expr_opt:
    /* nothing */ { Noexpr }
  | expr          { $1 }

expr:
    LITERAL          { Literal($1)            }
  | FLIT	     { Fliteral($1)           }
  | BLIT             { BoolLit($1)            }
  | SLIT             { StringLit($1)          }
  | ID               { Id($1)                 }
  | NULL             { NullLit                }
  | expr PLUS   expr { Binop($1, Add,   $3)   }
  | expr MINUS  expr { Binop($1, Sub,   $3)   }
  | expr TIMES  expr { Binop($1, Mult,  $3)   }
  | expr DIVIDE expr { Binop($1, Div,   $3)   }
  | expr MOD    expr { Binop($1, Mod,   $3)   }
  | expr EQ     expr { Binop($1, Equal, $3)   }
  | expr NEQ    expr { Binop($1, Neq,   $3)   }
  | expr LT     expr { Binop($1, Less,  $3)   }
  | expr LEQ    expr { Binop($1, Leq,   $3)   }
  | expr GT     expr { Binop($1, Greater, $3) }
  | expr GEQ    expr { Binop($1, Geq,   $3)   }
  | expr AND    expr { Binop($1, And,   $3)   }
  | expr OR     expr { Binop($1, Or,    $3)   }
  | MINUS expr %prec NEG { Unop(Neg, $2)      }
  | NOT expr         { Unop(Not, $2)          }
  | expr ASSIGN expr { Assign($1, $3)         }
  | ID LPAREN args_opt RPAREN { Call($1, $3)  }
  | LPAREN expr RPAREN { $2                   }
  | NEW nonprim_typ LPAREN args_opt RPAREN { New($2, $4)                    }
  | NEW nonprim_typ LBRACK LITERAL RBRACK  { NewArray($2, $4)               }
  | NEW prim_typ LBRACK LITERAL RBRACK     { NewArray($2, $4)               }
  | LBRACK args_opt RBRACK                 { CreateArray($2)                }
  | ID LBRACK expr RBRACK                  { AccessArray($1, $3)            }
  | ID LBRACK LITERAL COLON LITERAL RBRACK { SubArray($1, $3, $5)           }
  | expr DOT ID                            { AccessStruct($1, $3)           }
  | expr PLUS PLUS                         { PostUnop($1, PostIncrement)    }
  | expr MINUS MINUS                       { PostUnop($1, PostDecrement)    }

args_opt:
    /* nothing */ { [] }
  | args_list  { List.rev $1 }

args_list:
    expr                    { [$1] }
  | args_list COMMA expr { $3 :: $1 }
