open Printf

let rec _line0 () = if false then _line0 ()
let rec _line1 () = if false then _line1 ()
let rec _line2 () = if false then _line2 ()
let rec _line3 () = if false then _line3 ()
let rec _line4 () = if false then _line4 ()

module Comb = struct
  type stream = int
  type 'a r = Parsed of 'a * stream | Failed
  let (<|>) l r s =
    _line0 ();
    match l s with
      | (Parsed (_,_)) as ans -> ans
      | Failed -> r s

  let (-->) r f s = match r s with Parsed (x,s) -> Parsed (f x, s) | Failed -> Failed
  let (-->>>) r y s = match r s with Parsed (_,s) -> Parsed (y, s) | Failed -> Failed

  let opt p s =
    _line1();
    match p s with
      | Parsed (v,s') -> Parsed (Some v,s')
      | Failed        -> Parsed (None,  s)

  let lift s = Parsed ((),s)

  let seq p1 p2 s =
    _line2();
    match p1 s with
      | Parsed (x,s') -> p2 x s'
      | Failed -> Failed

  let (|>) = seq

  let (>>>) p1 p2 s =
    _line3 ();
    match p1 s with
      | Parsed (_, s') -> p2 s'
      | Failed   -> Failed

(*
  let manyFold f init p =
    let rec inner acc s =
      match p s with
        | Parsed (x, s') -> inner (f acc x) s'
        | Failed         -> Parsed (acc, s)
    in
    inner init

  let many p =
    (manyFold (fun acc x -> fun l -> acc (x::l)) (fun x -> x) p) --> (fun t -> t [])

  let list0 p delim =
    (p                           |> fun h ->
     many (delim |> fun _ -> p) --> fun tl ->
     (h::tl)
    )
    <|>
        (lift --> fun _ -> [])
      *)

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

let ps = OstapExprPrinter.printer

open Comb

let data_len = ref (-1)
let data = ref [||]

let look (s:string) (stream: Comb.stream) =
    if stream >= !data_len then Failed
    else match !data.(stream) with
      | (t,_,e,str) when s = str -> Parsed (str,stream+1)
      | _ -> Failed

let ident stream =
    if stream >= !data_len then Failed
    else match !data.(stream) with
      | (ExprYacc.IDENT _,s,e,str) -> Parsed (str,stream+1)
      | _ -> Failed

let literal stream =
    if stream >= !data_len then Failed
    else match !data.(stream) with
      | (ExprYacc.LITERAL _,s,e,str) -> Parsed (str,stream+1)
      | _ -> Failed

let comma stream =
    if stream >= !data_len then Failed
    else match !data.(stream) with
      | (ExprYacc.COMMA,s,e,str) -> Parsed (str,stream+1)
      | _ -> Failed

let lbra = look "("
let rbra = look ")"

let expr_    = ref (fun _ -> assert false)
let factor_  = ref (fun _ -> assert false)

let literal'' n =
  let n = int_of_string n in
  ps#literal n

let expr_in_bra' e = rbra -->>> e
let expr_in_bra =
  lbra >>> (!expr_ |> expr_in_bra')

let fun_call_p''''  fname args _ =
      ps#fun_call fname args
let fun_call_p'''  fname args =
      rbra                --> fun_call_p'''' fname args
let fun_call_p''  fname _ =
      (list0 !expr_ comma) |> (fun_call_p''' fname)
let fun_call_p' fname =
      lbra                 |> (fun_call_p'' fname)
let fun_call_p : Comb.stream -> Ostap.Pretty.printer Comb.r =
  ident                |> fun_call_p'

let primary =
    (* function call *)
    fun_call_p
    <|>
    (* variable *)
    (ident --> ps#ident)
    <|>
    (* literal *)
    (literal --> literal'')
    <|>
    (* (expr) *)
    expr_in_bra

let factor2 l r =
  match r with Some r -> ps#mul l r | None -> l
let factor1 l =
  opt (look "*" >>> !factor_) --> (factor2 l)
let factor =
    primary                              |> factor1

let expr2 l r =
  match r with Some r -> ps#add l r | None -> l
let expr1 l =
  (opt (look "+" >>> !expr_)) --> (expr2 l)
let expr =
    factor                              |>  expr1

let () =
  expr_ := expr;
  factor_ := factor

let program stream =
  let data_ = Array.init (String.length stream) (fun _ -> None) in
  let buf = Lexing.from_string stream in
  let count = ref 0 in
  let foo () =
    try
      while true do
        let ans =  LexerExpr.token buf in
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
    let data' = Array.init !count (fun _ -> (ExprYacc.EOF,-1,-1,"") ) in
    let u = ref 0 in
    Array.iter (fun item -> match item with Some p -> (data'.(!u) <- p; incr u) | None -> ()) data_;
    assert (!u = !count);
    data_len := Array.length data';

    data := data'
  in

  ((fun () -> (expr |> fun ans -> look ";" --> fun _ -> [ans]) 0),
   lexer_time)
