open Printf

let repr = Ostap.Matcher.Token.repr

module Comb = struct
  type stream = OstapLexerExpr.nomem
  type 'a r = Parsed of 'a * stream | Failed

  let (<|>) l r s =
    match l s with
      | (Parsed (_,_)) as ans -> ans
      | Failed -> r s

  let (-->) r f s = match r s with Parsed (x,s) -> Parsed (f x, s) | Failed -> Failed
  let (-->>>) r y s = match r s with Parsed (_,s) -> Parsed (y, s) | Failed -> Failed

  let opt p s =
    match p s with
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

let look (s:string) (stream: Comb.stream) =
  match stream#look s with
    | Ostap.Combinators.Parsed ((r,s),_) -> Parsed(r,s)
    | Ostap.Combinators.Failed _         -> Failed

let ident stream =
  match stream#getIDENT with
    | Ostap.Combinators.Parsed ((r,s),_) -> Parsed(repr r,s)
    | Ostap.Combinators.Failed _         -> Failed

let literal stream =
  match stream#getLITERAL with
    | Ostap.Combinators.Parsed ((r,s),_) -> Parsed(repr r,s)
    | Ostap.Combinators.Failed _         -> Failed

let comma stream = look "," stream

let lbra = look "("
let rbra = look ")"

let expr_    = ref (fun _ -> assert false)
let factor_  = ref (fun _ -> assert false)

let literal'' n =
  let n = int_of_string n in
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
let fun_call_p  =
  ident                |> fun_call_p'

let ident' x = Ast.Ident x

let primary =
    (* function call *)
    fun_call_p
    <|>
    (* variable *)
    (ident --> ident' )
    <|>
    (* literal *)
    (literal --> literal'')
    <|>
    (* (expr) *)
    expr_in_bra

let factor2 l r =
  match r with Some r -> Ast.Mul (l, r) | None -> l
let factor1 l =
  opt (look "*" >>> !factor_) --> (factor2 l)
let factor =
    primary                              |> factor1

let expr2 l r =
  match r with Some r -> Ast.Sum (l, r) | None -> l
let expr1 l =
  (opt (look "+" >>> !expr_)) --> (expr2 l)
let expr =
    factor                              |>  expr1

let () =
  expr_ := expr;
  factor_ := factor

let program stream =
  (fun () -> (expr |> fun ans -> look ";" --> fun _ -> [ans]) (new OstapLexerExpr.nomem stream))
