module Comb = struct
  type stream = int
  type 'a r = Parsed of 'a * stream | Failed
  let (<|>) l r s =
    match l s with
      | (Parsed (_,_)) as ans -> ans
      | Failed -> r s

  let (-->) r f s = match r s with Parsed (x,s) -> Parsed (f x, s) | Failed -> Failed

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
end

let ps = OstapExprPrinter.printer

open Comb

let expr_    = ref (fun _ -> assert false)
let factor_  = ref (fun _ -> assert false)

let program stream =
  let data = Array.init (String.length stream) (fun _ -> None) in
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
        data.(start) <- v
      done;
      assert false
    with OstapLexerExpr.Finished -> ()
  in
  let (_,lexer_time) = Helpers.eval_time_2 foo in

  let data : (ExprYacc.token * int * int * string) array =
    let data' = Array.init !count (fun _ -> (ExprYacc.EOF,-1,-1,"") ) in
    let u = ref 0 in
    Array.iter (fun item -> match item with Some p -> (data'.(!u) <- p; incr u) | None -> ()) data;
    assert (!u = !count);
    data'
  in
  let data_len = Array.length data in


  let look (s:string) (stream: Comb.stream) =
    if stream >= data_len then Failed
    else match data.(stream) with
      | (t,_,e,str) when s = str -> Parsed (str,stream+1)
      | _ -> Failed
  in
  let ident stream =
    if stream >= data_len then Failed
    else match data.(stream) with
      | (ExprYacc.IDENT _,s,e,str) -> Parsed (str,stream+1)
      | _ -> Failed
  in
  let literal stream =
    if stream >= data_len then Failed
    else match data.(stream) with
      | (ExprYacc.LITERAL _,s,e,str) -> Parsed (str,stream+1)
      | _ -> Failed
  in
  let comma stream =
    if stream >= data_len then Failed
    else match data.(stream) with
      | (ExprYacc.COMMA,s,e,str) -> Parsed (str,stream+1)
      | _ -> Failed
  in
  let lbra = look "(" in
  let rbra = look ")" in

  expr_    :=  (fun _ -> assert false);
  factor_  :=  (fun _ -> assert false);

  let primary =
    (* function call *)
    ( ident                |> fun fname ->
      lbra                 |> fun _ ->
      (list0 !expr_ comma) |> fun args ->
      rbra                --> fun _ ->
      ps#fun_call fname args
    )
    <|>
    (* variable *)
    ( ident --> fun id ->
      (*printf "ident %s parsed\n" (repr id);*)
      ps#ident ( id) )
    <|>
    (* literal *)
    ( literal --> fun n ->
      let n = int_of_string ( n) in
      (*printf "literal parsed: %d\n" n; *)
      ps#literal n )
    <|>
    (* (expr) *)
    (  lbra   |> fun _ ->
       !expr_ |> fun e ->
       rbra  --> fun _ ->
       e
    )
  in
  (*
     let f1 l r = (match r with Some r -> ps#mul l r | None -> l)     in
     let f2 l =   opt (look "*" |> fun _ -> !factor_) --> f1 l
     let f =
        primary                               |> f2
  *)


  let factor =
    primary                              |> fun l ->
    opt (look "*" |> fun _ -> !factor_) --> fun r ->
    (match r with Some r -> ps#mul l r | None -> l)
  in
  let expr =
    factor                              |> fun l ->
    (opt (look "+" |> fun _ -> !expr_)) --> fun r ->
    (match r with Some r -> ps#add l r | None -> l)
  in
  factor_ := factor;
  expr_ := expr;

  ((fun () -> (expr |> fun ans -> look ";" --> fun _ -> [ans]) 0),
   lexer_time)
