(** Ostap parser for arithmetic expressions language *)
open Printf

open Ostap.Util
open Ostap.Combinators
open Ostap
open Helpers

let make_reason msg l =
  (new Reason.t) (Msg.make msg [| |] (Matcher.Token.loc l))

let repr = Matcher.Token.repr

let ps = OstapExprPrinter.printer

let program ~memoize meth s =
  let expr = ref (fun _ -> assert false) in
  let primary = ref (fun _ -> assert false) in

  let bad_expr =
    let ostap (
       expr_:   l:factor "+" r:expr_ { ps#add l r }
              | factor ;
      factor:   l: !( !primary ) "*" r:factor { ps#mul l r }
              | l: !( !primary ) { l }
    ) in
    expr_
  in
  let good_expr = (* with left factorization *)
    let ostap (
       expr_:   l:factor         r:(-"+" expr_)?  { match r with Some r -> ps#add l r | None -> l } ;
      factor:   l: !( !primary ) r:(-"*" factor)? { match r with Some r -> ps#mul l r | None -> l }
    ) in
    expr_
  in

  let ident   = (fun __stream -> __stream#getIDENT) in
  let literal = (fun __stream -> __stream#getLITERAL) in
  let lbra    = (fun __stream -> __stream#look "(") in
  let rbra    = (fun __stream -> __stream#look ")") in
  let comma   = (fun __stream -> __stream#look ",") in

  let primary_ =
    (* function call *)
    ( ident              |> fun fname ->
      lbra               |> fun _ ->
      (Util.list0 !expr) |> fun args ->
      rbra              --> fun _ ->
      ps#fun_call (repr fname) args
    )
    <|>
    (* variable *)
    ( ident --> fun id ->
      (*printf "ident %s parsed\n" (repr id);*)
      ps#ident (repr id) )
    <|>
    (* literal *)
    ( literal --> fun n ->
      let n = int_of_string (repr n) in
      (*printf "literal parsed: %d\n" n; *)
      ps#literal n )
    <|>
    (* (expr) *)
    (  lbra   |> fun _ ->
       !expr  |> fun e ->
       rbra  --> fun _ ->
       e
    )
  in

  let () =
    match memoize,meth with
      | true,`Good     ->
          Ref.replace expr    (fun _ -> s#memoize0 "good_expr" good_expr);
          Ref.replace primary (fun _ -> s#memoize0 "primary"   primary_)
      | true,`Bad     ->
          Ref.replace expr    (fun _ -> s#memoize0 "expr"    bad_expr);
          Ref.replace primary (fun _ -> s#memoize0 "primary" primary_)
      | false,`Good   ->
          Ref.replace expr (fun _ -> good_expr);
          Ref.replace primary (fun _ -> primary_)
      | false,`Bad   ->
          Ref.replace expr (fun _ -> bad_expr);
          Ref.replace primary (fun _ -> primary_)
  in
  let program =
    let semi __stream = __stream#look ";" in
    (listBy semi  !expr) |> fun ans -> semi                --> fun _ -> ans
  in
  program s
