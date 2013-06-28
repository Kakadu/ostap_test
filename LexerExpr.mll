{
  open ExprYacc
  exception Eof
}
rule token = parse
            [' ' '\n' '\t']   { token lexbuf }
          | ['0'-'9']+ as lxm { LITERAL(int_of_string lxm) }
          | ['A'-'Z' 'a'-'z'] ['A'-'Z' 'a'-'z' '0'-'9'] * as id { IDENT id }
          | '+'               { PLUS }
          | '-'               { MINUS }
          | '*'               { TIMES }
          | '/'               { DIV   }
          | '('               { LPAREN }
          | ')'               { RPAREN }
          | '{'               { LBRACE }
          | '}'               { RBRACE }
          | ','               { COMMA }
          | ';'               { SEMICOLON }
          | eof               { EOF }
