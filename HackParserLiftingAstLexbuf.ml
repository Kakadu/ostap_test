open Printf

module Lexer = LexerExprAst

let input_string =  ref ""

module Comb = struct
  open Lexing
  type stream = Lexing.lexbuf

  let copy buf =
    let ans = Lexing.from_string !input_string in
(*    ans.lex_abs_pos    <- buf.lex_abs_pos; *)
    ans.lex_start_pos  <- buf.lex_start_pos;
    ans.lex_curr_pos   <- buf.lex_curr_pos;
(*    ans.lex_last_pos   <- buf.lex_last_pos;
    ans.lex_last_action<- buf.lex_last_action;
    ans.lex_eof_reached<- buf.lex_eof_reached;
    ans.lex_mem        <- buf.lex_mem;
    ans.lex_start_p    <- buf.lex_start_p;
    ans.lex_curr_p     <- buf.lex_curr_p; *)
    ans

  type 'a r = Parsed of 'a * stream | Failed

  let (<|>) l r s =
    match l s with
      | (Parsed (_,_)) as ans -> ans
      | Failed -> r s

  let (-->) r f s = match r s with Parsed (x,s) -> Parsed (f x, s) | Failed -> Failed
  let (-->>>) r y s = match r s with Parsed (_,s) -> Parsed (y, s) | Failed -> Failed

  let opt p s =
    let s' = copy s in
    match p s' with
      | Parsed (v,s') -> Parsed (Some v,s')
      | Failed        -> Parsed (None,  s)

  let lift s = Parsed ((),s)

  let seq p1 p2 s =
    match p1 s with
      | Parsed (x,s') -> p2 x s'
      | Failed -> Failed

  let (|>) = seq

  let (>>>) p1 p2 s =
    match p1 s with
      | Parsed (_, s') -> p2 s'
      | Failed   -> Failed

  let rec list0_loop (p,delim, acc, s) =
    match (delim >>> p) s with
      | Parsed (z, s') -> list0_loop (p, delim, z::acc, s')
      | Failed  -> (acc,s)

  let list0 p delim s =
    match p s with
      | Failed -> Failed
      | Parsed (h,s') ->
        begin
          let (ans,s) = list0_loop (p,delim,[h],s') in
          Parsed (List.rev ans, s)
        end

end

open Comb
(*
let data_len = ref (-1)
let data = ref [||]
  *)
let ident stream =
  let s' = copy stream in
  match Lexer.token s' with
    | ExprYaccAst.IDENT id -> Parsed (id, s')
    | _ -> Failed

let literal stream =
  let s' = copy stream in
  match Lexer.token s' with
    | ExprYaccAst.LITERAL str -> Parsed (str, s')
    | _ -> Failed

let comma stream =
  let s' = copy stream in
  match Lexer.token s' with
    | ExprYaccAst.COMMA -> Parsed ((), s')
    | _ -> Failed

let lbra stream =
  let s' = copy stream in
  match Lexer.token s' with
    | ExprYaccAst.LPAREN -> Parsed ((), s')
    | _ -> Failed

let rbra stream =
  let s' = copy stream in
  match Lexer.token s' with
    | ExprYaccAst.RPAREN -> Parsed ((), s')
    | _ -> Failed

let mul stream =
  let s' = copy stream in
  match Lexer.token s' with
    | ExprYaccAst.TIMES -> Parsed ((), s')
    | _ -> Failed

let plus stream =
  let s' = copy stream in
  match Lexer.token s' with
    | ExprYaccAst.PLUS -> Parsed ((), s')
    | _ -> Failed

let semi stream =
  let s' = copy stream in
  match Lexer.token s' with
    | ExprYaccAst.SEMICOLON -> Parsed ((), s')
    | _ -> Failed

let comma stream =
  let s' = copy stream in
  match Lexer.token s' with
    | ExprYaccAst.COMMA -> Parsed ((), s')
    | _ -> Failed


let expr_    = ref (fun _ -> assert false)
let factor_  = ref (fun _ -> assert false)

let literal'' n = (*
  let n = int_of_string n in *)
  Ast.Literal n

let expr_in_bra' e = rbra -->>> e
let expr_in_bra =
  lbra >>> (!expr_ |> expr_in_bra')

let fun_call_p''''  fname args _ =
  Ast.Call (fname, args)

let fun_call_p'''  fname args =
  rbra                --> fun_call_p'''' fname args
let fun_call_p''  fname _ =
  (list0 !expr_ comma) |> (fun_call_p''' fname)
let fun_call_p' fname =
  lbra                 |> (fun_call_p'' fname)
let fun_call_p : Comb.stream -> Ast.expr Comb.r =
  ident                |> fun_call_p'

let ident' x = Ast.Ident x

let primary'' name = fun () -> ident' name
let primary' name =
        (fun_call_p' name)
        <|>
        (lift --> (primary'' name))

let primary =
    (* function call *)
    ( ident |> primary' )
    <|>
    (* literal *)
    (literal --> literal'')
    <|>
    (* (expr) *)
    expr_in_bra

let factor2 l r =
  match r with Some r -> Ast.Mul (l, r) | None -> l
let factor1 l =
  opt (mul >>> !factor_) --> (factor2 l)
let factor =
    primary                              |> factor1

let expr2 l r =
  match r with Some r -> Ast.Sum (l, r) | None -> l
let expr1 l =
  (opt (plus >>> !expr_)) --> (expr2 l)
let expr =
    factor                              |>  expr1

let () =
  expr_ := expr;
  factor_ := factor

let program stream =
(*
  let data_ = Array.init (String.length stream) (fun _ -> None) in
  let buf = Lexing.from_string stream in
  let count = ref 0 in
  let foo () =
    try
      while true do
        let ans =  LexerExprAst.token buf in
        let start = Lexing.lexeme_start buf in
        let r = Lexing.lexeme_end buf in
        let v = Some (ans, start, r, Lexing.lexeme buf) in
        if start=r then raise OstapLexerExpr.Finished;
        incr count;
        data_.(start) <- v
      done;
      assert false
    with OstapLexerExpr.Finished -> ()
  in
  let (_,lexer_time) = Helpers.eval_time_2 foo in

  let () =
    let data' = Array.init !count (fun _ -> (ExprYaccAst.EOF,-1,-1,"") ) in
    let u = ref 0 in
    Array.iter (fun item -> match item with Some p -> (data'.(!u) <- p; incr u) | None -> ()) data_;
    assert (!u = !count);
    data_len := Array.length data';

    data := data'
  in *)
  input_string := stream;
  (fun () -> (expr |> fun ans -> semi --> fun _ -> [ans]) (Lexing.from_string !input_string))

