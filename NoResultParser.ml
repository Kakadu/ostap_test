open Printf
open Helpers

let rec _line0 () = if false then _line0 ()
let rec _line1 () = if false then _line1 ()
let rec _line2 () = if false then _line2 ()
let rec _line3 () = if false then _line3 ()
let rec _line4 () = if false then _line4 ()

module Comb = struct
  type stream = int
  type r  = { mutable parsed: bool; mutable result: unit; mutable stream: stream }

  let ans = { parsed = false; result = Obj.magic (); stream = -1; }
  let (<|>) l r s =
    l s;
    if not ans.parsed then r s

  let (-->) r f s =
    r s;
    if ans.parsed then ans.result <- Obj.magic (f ans.result)

  let (-->>>) r y s =
    r s;
    if ans.parsed then ans.result <- Obj.magic y

  let opt p s =
    p s;
    ans.result <- Obj.magic (if ans.parsed then Some ans.result else None);
    ans.parsed <- true

  let lift s = ans.parsed <- true; ans.result <- (); ans.stream <- s

  let seq p1 p2 s =
    printf "inside seq 1\n";
    p1 s;
    printf "inside seq 2\n";
    if ans.parsed then p2 (Obj.magic ans.result) ans.stream

  let (|>) = seq

  let (>>>) p1 p2 s : unit =
    p1 s;
    if ans.parsed then p2 ans.stream

  let rec list0_loop (p,delim, acc, s) =
    (delim >>> p) s;
    if ans.parsed then list0_loop (p,delim,  (ans.result) :: acc, ans.stream)
    else (acc,s)

  let list0 p delim s =
    p s;
    if ans.parsed then begin
      let (rrr,s) = list0_loop (p,delim, [ans.result], ans.stream) in
      ans.parsed <- true;
      ans.result <- Obj.magic (List.rev rrr);
      ans.stream <- s;
      ()
    end

end


let ps = OstapExprPrinter.printer

open Comb

let data_len = ref (-1)
let data = ref [||]

let parsed (str,stream) = ans.parsed <- true; ans.result <- Obj.magic str; ans.stream <- stream
let failed () = ans.parsed <- false

let look (s:string) (stream: Comb.stream) =
  printf "looking for string '%s' on pos %d\n%!" s stream;
    if stream >= !data_len then failed ()
    else match !data.(stream) with
      | (t,_,e,str) when s = str -> printf "string '%s' parsed\n" s; parsed (str,stream+1)
      | _                        -> failed ()

let ident stream =
    if stream >= !data_len then failed ()
    else match !data.(stream) with
      | (ExprYacc.IDENT _,s,e,str) -> printf "ident %s parsed\n" str; parsed (str,stream+1)
      | _ -> failed ()

let literal stream =
    if stream >= !data_len then failed ()
    else match !data.(stream) with
      | (ExprYacc.LITERAL _,s,e,str) -> printf "literal '%s' parsed\n" str;  parsed (str,stream+1)
      | _ -> failed ()

let comma stream =
    if stream >= !data_len then failed ()
    else match !data.(stream) with
      | (ExprYacc.COMMA,s,e,str) -> printf "comma parsed\n"; parsed (str,stream+1)
      | _ -> failed ()

let lbra = look "("
let rbra s =
  print_endline "inside rbra";
  look ")" s

let expr_    = ref (fun _ -> assert false)
let factor_  = ref (fun _ -> assert false)

let literal'' n =
  let n = int_of_string n in
  ps#literal n

let expr_in_bra' e = rbra -->>> e
let expr_in_bra =
  lbra >>> (!expr_ |> expr_in_bra')

let fun_call_p''''  fname args () stream =
  let s = ps#fun_call fname args in
  parsed (s, stream)

let fun_call_p'''  fname args =
  printf "inside fun_call_p'''\n%!";
  rbra                |> (fun_call_p'''' fname args)
let fun_call_p''  fname _ s =
  printf "inside fun_call_p''\n%!";
  ((list0 !expr_ comma) |> (fun_call_p''' fname)) s;
  printf "exit fun_call_p''\n"
let fun_call_p' fname s =
  printf "inside fun_call_p'\n";
  (lbra                 |> (fun_call_p'' fname)) s;
  printf "exit fun_call_p'\n"

let fun_call_p : Comb.stream ->  unit =
  ident                |> fun_call_p'

let primary =
    (* function call *)
    fun_call_p
    <|>
    (* variable *)
    (ident --> (fun x -> ps#ident (Obj.magic x) ) )
    <|>
    (* literal *)
    (literal --> (fun x -> literal'' (Obj.magic x) ) )
    <|>
    (* (expr) *)
    expr_in_bra

let factor2 l r =
  match Obj.magic r with Some r -> ps#mul l r | None -> l
let factor1 l =
  opt (look "*" >>> !factor_) --> (factor2 l)

let factor s =
  (primary                               |> factor1) s


let expr2 l r =
  match Obj.magic r with Some r -> ps#add l r | None -> l
let expr1 l =
  (opt (look "+" >>> !expr_)) --> (expr2 l)
let expr s =
  (factor                        |>  expr1) s;
  ()

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

  ((fun () -> (expr |> (fun p ->
    printf "Hashtbl.hash p = %d\n%!" (Hashtbl.hash p);
    (*
    let p : Ostap.Pretty.printer = Obj.magic ans.result in *)
    printf "EXPR ended(%b): %s\n%!" ans.parsed (Ostap.Pretty.toString (Obj.magic p));
    look ";" --> fun _ -> [p])) 0),
   lexer_time)
