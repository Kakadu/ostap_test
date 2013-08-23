(*
   In this scannerless recusive descent. Without any lexbuf

*)

open Helpers
open Printf


module Lexer = struct
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
end
(*
type state = {
  mutable input: string;
  mutable buf: Lexer.lexbuf; (* it is latest buffer state *)
} *)
(*
let st = {
  input = "";
  buf = Lexer.from_string "";
}
  *)

open Lexer

let getp = function Parsed (x,y) -> (x,y) | Failed -> failwith "Bad argument of getp"

let rec arguments (acc, buf) =
  let (l,buf) = expr buf in
  match comma buf with
    | Parsed((),buf) ->  arguments (l::acc, buf)
    | Failed         ->  (List.rev (l::acc), buf)

and primary (buf: Lexer.lexbuf) =
  if try_literal buf then begin
    let (n,buf) = getp (literal buf) in
    (Ast.Literal n, buf)
  end else
    match lbra buf with
      | Parsed ((),buf) -> begin
          let (e,buf) = (expr buf)  in
          match rbra buf with
            | Parsed ((),buf) -> (e,buf)
            | Failed -> failwith "Missing ')'"
      end
      | Failed -> begin
        if not (try_ident buf) then failwith "missing primary expression";
        let (id, buf) = ident buf |! getp in
        match lbra buf with
          | Parsed((),buf) -> begin
              let args,buf = arguments ([], buf) in
              match rbra buf with
                | Parsed ((),buf) -> (Ast.Call (id,args), buf)
                | Failed -> failwith "mising ')'"
          end
          | Failed -> (Ast.Ident id, buf)
      end

and factor buf =
  let (l,buf) = primary buf in
  if is_finished buf then (l,buf)
  else match mul buf with
    | Parsed ((), buf) ->
        if is_finished buf then failwith "factor expected" (* factor expected *)
        else let (r,buf) = factor buf in
             (Ast.Mul (l, r), buf)
    | Failed -> (l, buf)

and expr (buf: Lexer.lexbuf) =
  let (l, buf) = factor buf in
  if is_finished buf then (l,buf)
  else match plus buf with
    | Parsed ((),buf) ->
      let (r,buf) = expr buf in
      (Ast.Sum (l,r), buf)
    | ___ -> (l,buf)

let statement buf =
  let (e,buf) = expr buf in
  match semi buf with
    | Parsed((),_) -> e
    | _______ -> failwith "';' expected"

let main s () =
  let buf = Lexer.from_string s in
  try
    let ans = statement buf in
    [ans]
  with exc ->
    Printexc.to_string exc |! print_endline;
    Printexc.get_backtrace () |!  (print_endline);
    raise exc

let program stream = main stream



