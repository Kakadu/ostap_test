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


let util_expr f ops opnd =
  let left  f c x y = f (c x) y in
  let right f c x y = f x y in

  let ops =
    Array.map
      (fun (assoc, list) ->
        let g = match assoc with `Lefta | `Nona -> left | `Righta -> right in
        (assoc = `Nona), altl (List.map (fun (oper, sema) -> ostap (!(oper) {g sema})) list)
      )
      ops
  in
  let n      = Array.length ops in
  let op   i = snd ops.(i)      in
  let nona i = fst ops.(i)      in
  let id x   = x                in
  let ostap (
    inner[l][c]: f[ostap (
      {n = l                } => x:opnd {c x}
    | {n > l && not (nona l)  && (printf "2nd variant, l=%d\n" l; true) }
                              => x:inner[l+1][id] b:(-o:op[l] inner[l][o c x])? {
        match b with None -> c x | Some x -> x
      }
    | {n > l && nona l && (printf "3rd variant\n"; true)}
                              => x:inner[l+1][id] b:(-o:op[l] inner[l+1][o c x])? {
        match b with None -> c x | Some x -> x
      })]
  )
  in
  ostap (inner[0][id])

let program s =
  let expr = ref (fun _ -> assert false) in
  let primary = ref (fun _ -> assert false) in

  let expr_ s =
    util_expr Util.id
      [| `Lefta,
         [(fun __stream -> printf "looking for '+' at pos %d\n" __stream#pos; __stream#look "+"),  ps#add;
          (fun __stream -> __stream#look "-"),  ps#sub];
         `Lefta,
         [(fun __stream -> printf "looking for '*' at pos %d\n" __stream#pos; __stream#look "*"),  ps#mul;
          (fun __stream -> __stream#look "/"),  ps#divide]
      |]
      !primary s
  in

  let ident   = (fun __stream -> __stream#getIDENT) in
  let literal = (fun __stream -> __stream#getLITERAL) in
  let lbra    = (fun __stream -> __stream#look "(") in
  let rbra    = (fun __stream -> __stream#look ")") in
  let comma   = (fun __stream -> __stream#look ",") in

  let primary_ = (*
    (* 2-arg function call *)
    ( ident              |> fun fname ->
      lbra               |> fun _ ->
      (!expr)            |> fun arg1 ->
      comma              |> fun _ ->
      (!expr)            |> fun arg2 ->
      comma              |> fun _ ->
      (!expr)            |> fun arg3 ->
      comma              |> fun _ ->
      (!expr)            |> fun arg4 ->
      comma              |> fun _ ->
      (!expr)            |> fun arg5 ->
      rbra               --> fun _ -> (*
        print_endline "5-arg function"; *)
      ps#fun_call (repr fname) [arg1;arg2;arg3;arg4;arg5]
    )
    <|> *)
    (* function call *)
    ( ident              |> fun fname ->
      lbra               |> fun _ ->
      (Util.list0 !expr) |> fun args ->
      rbra              --> fun _ -> (*
        print_endline "n-arg function"; *)
      ps#fun_call (repr fname) args
    )
    <|>
    (* variable *)
    ( ident --> (fun id -> printf "ident %s parsed\n" (repr id); ps#ident (repr id)) )
    <|>
    (* literal *)
    ( literal --> (fun n -> let n = int_of_string (repr n) in
                            printf "literal parsed: %d\n" n;
                            ps#literal n ) )
    <|>
    (* (expr) *)
    (  lbra   |> fun _ ->
       !expr  |> fun e ->
       rbra  --> fun _ ->
       e
    )
  in
  let (_: _ -> (_,_,_) Ostap.Combinators.result) = expr_ in
  let () =
    let mem = true in
    if mem then begin
      let () = Ref.replace expr    (fun _ -> s#memoize0 "expr"    expr_) in
      let () = Ref.replace primary (fun _ -> s#memoize0 "primary" primary_) in
      printf "memoization\n"
    end else begin
      let () = Ref.replace expr (fun _ -> expr_) in
      let () = Ref.replace primary (fun _ -> primary_) in
      ()
    end
  in
  let program =
    let semi = (fun _ostap_stream -> _ostap_stream#look ";") in
    (listBy semi  !expr) |> (fun ans -> semi                --> (fun _ -> ans) )
  in
  program s
