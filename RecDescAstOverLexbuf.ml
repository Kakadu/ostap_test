open Helpers
open Printf

module Lexer = LexerExprAst

type state = {
  mutable input: string;
  mutable buf: Lexing.lexbuf;
  mutable curToken: ExprYaccAst.token;
  mutable nextToken: ExprYaccAst.token;
}

let st = { input=""; buf = Lexing.from_string ""; curToken=ExprYaccAst.EOF ; nextToken=ExprYaccAst.EOF }
(*
let scanner = ref (-1)
let nextToken () = incr scanner
let curToken () = !data.(!scanner)
let finished () = !scanner >= !data_len *)
let nextToken () =
  st.curToken  <- st.nextToken;
  st.nextToken <- Lexer.token st.buf

let finished () = st.curToken = ExprYaccAst.EOF

let rec arguments acc =
  let l = expr () in
  match st.curToken with
    | ExprYaccAst.COMMA -> nextToken (); arguments (l::acc)
    | _ -> List.rev (l::acc)

and primary () =
  match st.curToken with
    | ExprYaccAst.LITERAL n ->
         nextToken ();
         Ast.Literal n
    | ExprYaccAst.LPAREN    -> begin
          nextToken ();
          let e = expr () in
          match st.curToken with
            | ExprYaccAst.RPAREN -> nextToken (); e
            | _____ -> failwith "Missing ')'"
    end
    | ExprYaccAst.IDENT id when st.nextToken = ExprYaccAst.EOF -> Ast.Ident id
    | ExprYaccAst.IDENT id -> begin (* function call *)
         match st.nextToken with
           | ExprYaccAst.LPAREN ->
               nextToken ();
               nextToken ();
               let args = arguments [] in
               begin
                 match st.curToken with
                   | ExprYaccAst.RPAREN -> nextToken(); Ast.Call (id, args)
                   | _ -> failwith (sprintf "Missing ')' " )
               end
           | _ -> Ast.Ident id
    end
    | _ -> failwith (sprintf "missing primary expression" )

and factor () =
  let l = primary () in
  if finished () then l
  else
  match st.curToken with
    | ExprYaccAst.TIMES ->
      begin
        nextToken ();
        if finished () then failwith "factor expected" else
          let r = factor () in
          Ast.Mul (l, r)
      end
    | ___ -> l

and expr () =
  let l = factor () in
  if finished () then l
  else (*
    let () = print_endline "checking for +" in *)
    match st.curToken with
      | ExprYaccAst.PLUS ->
        begin
          nextToken ();
          let r = expr () in
          Ast.Sum (l,r)
        end
      | ___ -> l

let statement () =
  let e = expr () in
  if finished () then failwith "';' expected" else nextToken ();
  e

let main s () =
  st.input <- s;
  st.buf   <- Lexing.from_string s;
  st.curToken <- Lexer.token st.buf;
  if not(finished ()) then st.nextToken <- Lexer.token st.buf;
  try
    let ans = statement () in
    [ans]
  with exc ->
    Printexc.to_string exc |! print_endline;
    raise exc

let program stream = main stream



