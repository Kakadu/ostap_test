open Helpers

  type lexbuf = {
    pos: int;
    str: string;
    str_len: int
  }
  type 'a r = Parsed of 'a * lexbuf | Failed
  let from_string s = { pos=0; str=s; str_len=String.length s }
  let is_finished b = (b.pos >= b.str_len)
  (* returns position where to start parsing next *)
  let rec ident' (pos,acc,buf) =
    if is_finished buf then pos
    else
      let c = buf.str.[pos] in
      match c with
        | '0'..'9'
        | 'a'..'z'
        | 'A'..'Z' -> Buffer.add_char acc c; ident' (pos+1, acc,buf)
        | _ -> pos

  let ident buf =
    if is_finished buf then Failed
    else
      let c = buf.str.[buf.pos] in
      match c  with
        | 'a'..'z'
        | 'A'..'Z' ->
          let sbuf = Buffer.create 5 in
          Buffer.add_char sbuf c;
          let next_pos = ident' (buf.pos+1,sbuf,buf) in
          Parsed (Buffer.contents sbuf, {buf with pos=next_pos} )
        | _ -> Failed

  let try_ident buf =
    if is_finished buf then false
    else match buf.str.[buf.pos] with
      | 'A'..'Z'
      | 'a'..'z' -> true
      | _ -> false

  (* returns position where to start parsing next *)
  let rec literal' (pos,acc,buf) =
    if is_finished buf then pos
    else
      let c = buf.str.[pos] in
      match c with
        | '0'..'9' -> Buffer.add_char acc c; literal' (pos+1, acc, buf)
        | _ -> pos

  let literal buf =
    if is_finished buf then Failed
    else
      let c = buf.str.[buf.pos] in
      (* TODO: add whitespace *)
      match c  with
      | '0'..'9' ->
           let sbuf = Buffer.create 5 in
           Buffer.add_char sbuf c;
           let next_pos = literal' (buf.pos+1,sbuf,buf) in
           Parsed (Buffer.contents sbuf |! int_of_string, {buf with pos=next_pos})
      | _ -> Failed

  let try_literal buf =
    if is_finished buf then false
    else match buf.str.[buf.pos] with
      | '0'..'9' -> true
      | _ -> false

  let single_char_parser ch buf =
    if is_finished buf then Failed
    else
      if buf.str.[buf.pos] = ch then Parsed((),{buf with pos=buf.pos+1})
      else Failed

  let comma = single_char_parser ','
  let lbra  = single_char_parser '('
  let rbra  = single_char_parser ')'
  let semi  = single_char_parser ';'
  let plus  = single_char_parser '+'
  let times = single_char_parser '*'
  let mul   = times
