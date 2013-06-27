%{

let ps = ref PrinterYacc.printer
let set_printer o = Helpers.Ref.replace ps (fun _ -> o)
%}
%token <int> LITERAL
%token <string> IDENT
%token GLOBALS BEGIN END PRINT_INT
%token PLUS MINUS TIMES DIV EQ
%token LPAREN RPAREN LBRACE RBRACE
%token COMMA SEMICOLON
%left  PLUS MINUS        /* lowest precedence */
%left  TIMES DIV         /* medium precedence */
%start main
%type <Ostap.Pretty.printer> main
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
  IDENT EQ expr                SEMICOLON { !ps#assign $1 $3 }
| PRINT_INT LPAREN expr RPAREN SEMICOLON { !ps#print_int $3 }
| expr                         SEMICOLON { !ps#statement $1 }
;
statements:
  statement statements                   { $1 :: $2 }
| statement                              { [$1]     }
;
func_heading:
  IDENT LPAREN RPAREN                    { !ps#declare_locals []; ($1, []) }
| IDENT LPAREN comma_ident_list RPAREN   { !ps#declare_locals $3; ($1, $3) }
;

func:
  func_heading LBRACE statements RBRACE
  {
    !ps#erase_locals ();
    let (name,args) = $1 in
    !ps#func (name, args, $3)
  }
;
functions:
  func functions { $1 :: $2 }
|                {    []    }
;
main_prog:
  BEGIN statements END                   { $2 }
;
global_vars:
  GLOBALS comma_ident_list END           { !ps#declare_globals $2; $2 }
|                                        { [] }
;
main:
  global_vars functions main_prog   {  !ps#program $1 $2 $3  }
;
%%

