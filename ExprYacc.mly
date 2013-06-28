%{

let ps = ref OstapExprPrinter.printer
let set_printer o = Helpers.Ref.replace ps (fun _ -> o)
%}
%token <int> LITERAL
%token <string> IDENT
%token GLOBALS BEGIN END PRINT_INT
%token PLUS MINUS TIMES DIV EQ
%token LPAREN RPAREN LBRACE RBRACE
%token COMMA SEMICOLON
%token EOF
%left  PLUS MINUS        /* lowest precedence */
%left  TIMES DIV         /* medium precedence */
%start program
%type <Ostap.Pretty.printer list> program
%type <Ostap.Pretty.printer> expr
%type <string list> comma_ident_list
%%
comma_ident_list:
  IDENT COMMA comma_ident_list     { $1 :: $3 }
| IDENT                            {   [$1]   }
;
comma_expr_list:
  expr COMMA comma_expr_list       { $1 :: $3 }
| expr                             { [$1] }
|                                  { [] }
;
expr:
  LITERAL                                { !ps#literal $1 }
| IDENT LPAREN RPAREN                    { !ps#fun_call $1 [] }
| IDENT LPAREN comma_expr_list RPAREN    { !ps#fun_call $1 $3 }
| IDENT                                  { !ps#ident $1 }
| LPAREN expr RPAREN                     { $2 }
| expr PLUS expr                         { !ps#add $1 $3 }
| expr MINUS expr                        { !ps#sub $1 $3 }
| expr TIMES expr                        { !ps#mul $1 $3 }
| expr DIV expr                          { !ps#divide $1 $3 }
;
statement:
| expr                         SEMICOLON { !ps#statement $1 }
;
statements:
  statement statements                   { $1 :: $2 }
| statement                              { [$1]     }
;
program:
  statements EOF { $1 }
;
%%

