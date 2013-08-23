module Token = ExprYaccAst

(*
   My simple lexer for expression language. with mutable lexbuf. is going to use in yacc
*)
type lexbuf = {
  mutable pos: int;
          str: string;
      str_len: int
}

let lexbuf_from_string str = { pos=0; str=str; str_len = String.length str }
let nextPos buf = buf.pos <- buf.pos + 1

let rec literal (acc, buf) =
  if buf.pos >= buf.str_len then acc
  else match buf.str.[buf.pos] with
    | '0'..'9' as c ->
      let x = acc*10 + (int_of_char c - int_of_char '0') in
      nextPos buf;
      literal (x, buf)
    | _ -> acc

let rec ident (acc, buf) =
  if buf.pos >= buf.str_len then acc
  else
    let c = buf.str.[buf.pos] in
    match buf.str.[buf.pos] with
    | '0'..'9'
    | 'a'..'z'
    | 'A'..'Z' -> Buffer.add_char acc c; nextPos buf; ident (acc,buf)
    | _ -> acc

exception EmptyToken
let rec token buf =
  if buf.pos >= buf.str_len then Token.EOF
  else
    let c = buf.str.[buf.pos] in
    match c  with
      | ' ' | '\t' | '\n' -> (nextPos buf; token buf)
      | '('  ->  nextPos buf; Token.LPAREN
      | ')'  ->  nextPos buf; Token.RPAREN
      | ','  ->  nextPos buf; Token.COMMA
      | ';'  ->  nextPos buf; Token.SEMICOLON
      | '+'  ->  nextPos buf; Token.PLUS
      | '*'  ->  nextPos buf; Token.TIMES
      | '0'..'9' ->
           nextPos buf;
           Token.LITERAL ( literal (int_of_char c - int_of_char '0', buf) )
      | 'a'..'z'
      | 'A'..'Z' ->
           nextPos buf;
           let sbuf = Buffer.create 5 in
           Buffer.add_char sbuf c;
           ident (sbuf,buf);
           Token.IDENT (Buffer.contents sbuf)
      | _ -> raise EmptyToken




