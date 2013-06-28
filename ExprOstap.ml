(** Ostap parser for arithmetic expressions language *)
open Printf
open Ostap
open Util
open Combinators
open Helpers

let make_reason msg l = new Reason.t (Msg.make msg [||] (Matcher.Token.loc l))
let repr = Matcher.Token.repr

let ps = OstapExprPrinter.printer
let program s =
  let expr    = ref (fun _ -> assert false) in
  let primary = ref (fun _ -> assert false) in
  let expr_  s =
    Util.expr Util.id
      [| `Lefta, [ ostap ("+"), ps#add
                 ; ostap ("-"), ps#sub     ]
       ; `Lefta, [ ostap ("*"), ps#mul
                 ; ostap ("/"), ps#divide  ]
      |]
      !primary
      s
  and ostap (    (* TODO: backtracking below *)
  primary_:
      fname:IDENT "(" args: !(Util.list0)[(!expr)] ")"
                      { ps#fun_call (repr fname) args }

    | id:IDENT    { ps#ident  (repr id) }
    | n:LITERAL   { ps#literal (repr n |! int_of_string)  }
    | -"(" !(!expr) -")"
  )
  in
(*
  let () = Ref.replace expr    (fun _ -> s#memoize0 expr_) in
  let () = Ref.replace primary (fun _ -> s#memoize0 primary_) in
*)
  let () = Ref.replace expr    (fun _ ->  expr_) in
  let () = Ref.replace primary (fun _ ->  primary_) in

  let ostap (
    program: listBy[ostap(";")][(!expr)]
  ) in
  program s
