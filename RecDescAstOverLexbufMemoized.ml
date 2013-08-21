open Helpers
open Printf

(*
   In this recusive descent we will save lexing results to allow possible backtracking.
   Than we compare executing speed we non-memization recurtsive descent.
*)

module Lexer = LexerExprAst

module HashableInt = struct
  type t = int
  let equal (x:int) (y:int) = (x=y)
  let compare (x:int) (y:int) = compare x y
  let hash x = x
end

module H=Hashtbl.Make(HashableInt)

type state = {
      container: ExprYaccAst.token H.t;
  mutable input: string;
  mutable buf: Lexing.lexbuf; (* it is latest buffer state *)
                              (* buf.Lexing.lex_curr_pos is position in the file *)
  mutable curToken: ExprYaccAst.token;
  mutable nextToken: ExprYaccAst.token;
}


(*
   большой вопрос надо ли мне в структуре иметь nextToken ибо у меня как бы есть хештаблица.

   Положим мы будем хранить в структуре последнее состояние лексбуфа (а в нём будет максимальная позиция m)
   если нам надо распарсить что-то по смещению <m, то это либо есть в хэш таблице, либо пустота и бага.

   из-за хештаблицы у нас будет и lookahead, так что поле nextToken не нужно
   функция будет вида lookahead: int -> token

   Остается вопрос как посмотрев в хештаблицу вычислить номер следующей позиции не храня её там, ибо
   просто прибавить длину токена не получается из-за whitespace
*)
let st = {
  container = H.create 1000000;
  input=""; buf = Lexing.from_string ""; curToken=ExprYaccAst.EOF ; nextToken=ExprYaccAst.EOF }

let nextToken () =
  st.curToken  <- st.nextToken;
  let pos = st.buf.Lexing.lex_curr_pos in
  st.nextToken <- Lexer.token st.buf;
  H.add st.container pos st.nextToken

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
  H.add st.container 0 st.curToken;

  if not(finished ()) then st.nextToken <- Lexer.token st.buf;
  try
    let ans = statement () in
    [ans]
  with exc ->
    Printexc.to_string exc |! print_endline;
    raise exc

let program stream = main stream



