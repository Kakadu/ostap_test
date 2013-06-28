open Ostap.Util
open Ostap.Combinators
open Ostap.Pretty
open Helpers
open Printf


let () =
  let source  = read (Sys.argv.(1)) in
  let lexer = new MarkdownLexer.t source in
  match (fun () -> Lang3.program lexer) |! eval_time "Ostap markdown parsing" with
  | Parsed ((xs,_),_) ->
    with_file "ostap_md.html" (fun ch ->  xs |! Ostap.Pretty.toString  |! fprintf ch "%s\n")
  | Failed r     ->
      printf "TT\n";
      Ostap.Reason.toString `All `Desc r |! print_endline;
      exit 1

