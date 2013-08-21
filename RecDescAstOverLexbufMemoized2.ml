open Helpers
open Printf

(*
   In this recusive descent we will save lexing results to allow possible backtracking.
   Than we compare executing speed we non-memization recurtsive descent.
*)

module Lexer = LexerExprAst
module Token = ExprYaccAst

let string_of_token = function
  | Token.IDENT s -> "IDENT " ^ s
  | Token.LITERAL n -> sprintf "LITERAL %d" n
  | Token.TIMES -> "TIMES"
  | Token.PLUS -> "PLUS"
  | Token.LPAREN ->  "LPAREN"
  | Token.RPAREN ->  "RPAREN"
  | Token.COMMA ->  "COMMA"
  | Token.SEMICOLON ->  "SEMI"
  | Token.EOF ->  "EOF"

module HashableInt = struct
  type t = int
  let equal (x:int) (y:int) = (x=y)
  let compare (x:int) (y:int) = compare x y
  let hash x = x
end

module H=Hashtbl.Make(HashableInt)

type state = {
      container: (ExprYaccAst.token*int) H.t;
  mutable input: string;
  mutable buf: Lexing.lexbuf; (* it is latest buffer state *)
                              (* buf.Lexing.lex_curr_pos is position in the file after last parsed token *)

  mutable curPos: int;
  mutable curToken: ExprYaccAst.token;
  mutable lastParsedPos: int;
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
  input = "";
  buf = Lexing.from_string "";
  curPos = -1;
  curToken = ExprYaccAst.EOF;
  lastParsedPos = -1;
}

let save pos ((tok,nextpos) as what) = (*
  printf "Saving at (%s,%d) pos %d\n%!" (string_of_token tok) nextpos pos;*)
  H.add st.container pos what

(* [tokenAt pos] returns cached token at position [pos] *)
let tokenAt ~pos =
  (*printf "Trying to find something on pos %d\n%!" pos;*)
  H.find st.container pos

let mycmp_int (x:int) (y:int) = match compare x y with
  | -1 -> `Less
  |  0 -> `Eq
  |  1 -> `Larger
  | __ -> assert false

(* return None if we can't look ahead so deep and [Some pos] if we can where pos is stream position/hashtable key
   to get needed token
*)
let can_la n =
  let rec helper (n,pos) =
    match (n, mycmp_int pos st.lastParsedPos) with
      | (0, `Less) -> Some pos
      | (_, `Less) ->
           let (_,next_pos) = tokenAt ~pos in
           helper (n-1, next_pos)
      | (0, `Eq)   -> Some pos
      | (_, `Eq)   ->
            (* We need to check if we can grab next token *)
            let (next_token,next_pos) = tokenAt ~pos in
            if next_token = Token.EOF
            then None
            else begin
              st.lastParsedPos <- st.buf.Lexing.lex_curr_pos;
              let tok = Lexer.token st.buf in
              save st.lastParsedPos (tok, st.buf.Lexing.lex_curr_pos);
              helper (n,pos)
            end
      | (_, `Larger) -> assert false
  in
  helper (n, st.curPos)

let nextToken () =
  assert (st.curToken <> Token.EOF);
  let (_, nextPos) = tokenAt ~pos:st.curPos in
  match mycmp_int nextPos st.buf.Lexing.lex_curr_pos with
    | `Eq -> begin
      st.curPos <- nextPos;
      let tok = Lexer.token st.buf in
      save nextPos (tok, st.buf.Lexing.lex_curr_pos);
      st.curToken <- tok;
      st.lastParsedPos <- st.curPos
    end
    | `Less -> begin
      st.curPos <- nextPos;
      st.curToken <- (tokenAt ~pos:nextPos) |! fst
    end
    | `Larger -> assert false

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
    | ExprYaccAst.IDENT id when can_la 2 = None -> Ast.Ident id
    | ExprYaccAst.IDENT id -> begin (* function call *)
         let nextPos = match can_la 1 with Some x -> x | _ -> assert false in
         match fst (tokenAt ~pos:nextPos) with
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
  (*printf "curToken <- %s\n" (string_of_token st.curToken);*)
  st.lastParsedPos <- 0;
  st.curPos <- 0;
  save 0 (st.curToken, st.buf.Lexing.lex_curr_pos);

  try
    let ans = statement () in
    [ans]
  with exc ->
    Printexc.to_string exc |! print_endline;
    Printexc.get_backtrace () |!  (print_endline);
    raise exc

let program stream = main stream



