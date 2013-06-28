(** Ostap parser for small language *)
open Printf
open Ostap
open Combinators
open Helpers

type text_kind = Bold | Italic | Normal
type resolver = string -> text_kind option
let make_reason msg l = new Reason.t (Msg.make msg [||] (Matcher.Token.loc l))
let repr = Matcher.Token.repr
let ps = MarkdownPrinter.printer

let program s =
  let ostap (
    start[n]: $ { printf "starting rule %d\n%!" n };
    fin[n]:   $ { printf "exiting  rule %d\n%!" n };

    expression:
      start[1] "_"  es : expression+ "_" fin[1] {
        printf "rule 1: %s\n%!" Pretty.(seq es |! toString);
        (ps#italic es)
      } (*
    | start[2] "_"  es : expression* fin[2] {
        printf "rule 2: %s\n%!" Pretty.(seq es |! toString);
        ( ps#seq [ps#text "_"] es)
    }

    | start[3] "*"  es : expression[r]+ "*" fin[3] {
        let es = List.map snd es in
        printf "rule 3: %s\n%!" Pretty.(seq es |! toString);
        (r, ps#bold  es)
    }
    | start[4] "*" es : expression[r]* fin[4] {
        let es = List.map snd es in
        printf "rule 4: %s\n%!" Pretty.(seq es |! toString);
        (r, ps#seq [ps#text "*"] ( es))
      } *)
    | start[5] text:IDENT fin[5] {
             print_endline ("rule 5 with " ^ (repr text));
             (ps#text (repr text) )
    }
    ;
    p:
      e:expression { e }
  )
  in
  p s
