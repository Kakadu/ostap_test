(*
   In this scannerless recusive descent. Without any lexbuf

*)

open Helpers
open Printf

module Lexer = ScannerlessLexerCore
open Lexer

let getp = function Parsed (x,y) -> (x,y) | Failed -> failwith "Bad argument of getp"

let rec arguments (acc, buf) =
  let (l,buf) = expr buf in
  match comma buf with
    | Parsed((),buf) ->  arguments (l::acc, buf)
    | Failed         ->  (List.rev (l::acc), buf)

and primary (buf: lexbuf) =
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

and expr buf =
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
