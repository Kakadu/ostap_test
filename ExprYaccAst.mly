%{
open Ast

%}
%token <int> LITERAL
%token <string> IDENT
%token PLUS TIMES
%token LPAREN RPAREN
%token COMMA SEMICOLON
%token EOF
%right  PLUS         /* lowest precedence */
%right  TIMES        /* medium precedence */
%start program
%type <Ast.expr list> program
%type <Ast.expr> expr
%%

comma_expr_list:
  expr COMMA comma_expr_list       { $1 :: $3 }
| expr                             { [$1] }
|                                  { [] }
;
expr:
  LITERAL                                { Literal $1 }
| IDENT LPAREN RPAREN                    { Call ($1, []) }
| IDENT LPAREN comma_expr_list RPAREN    { Call ($1, $3) }
| IDENT                                  { Ident $1}
| LPAREN expr RPAREN                     { $2 }
| expr PLUS expr                         { Sum ($1, $3) }
| expr TIMES expr                        { Mul ($1, $3) }
;
statement:
| expr                         SEMICOLON { $1 }
;
statements:
  statement statements                   { $1 :: $2 }
| statement                              { [$1]     }
;
program:
  statements EOF { $1 }
;
%%

