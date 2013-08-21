{
  open ExprYaccAst
  exception Eof
}
rule token = parse
            [' ' '\n' '\t']   { token lexbuf }
          | ['0'-'9']+ as lxm { LITERAL(int_of_string lxm) }
          | ['A'-'Z' 'a'-'z'] ['A'-'Z' 'a'-'z' '0'-'9'] * as id { IDENT id }
          | '+'               { PLUS }
          | '*'               { TIMES }
          | '('               { LPAREN }
          | ')'               { RPAREN }
          | ','               { COMMA }
          | ';'               { SEMICOLON }
          | eof               { EOF }
