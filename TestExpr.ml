open Helpers
open Printf
open Ostap.Util
open Ostap.Combinators


type options = {
  mutable filename  : string;
  mutable with_yacc : bool;
  mutable with_ostap: bool;
}
let ostap_output_file =     "ostap.expr.out"
let yacc_output_file  =     "yacc.expr.out"

let () =
  Gc.(
    let c = get () in
    c.minor_heap_size <- 26214400;
    c.max_overhead <- 1000000;
    set c
  )

let options = { filename="expr2.e"; with_yacc=false; with_ostap=true }

let () =
  Arg.parse [ ("-f", Arg.String (fun s -> options.filename <- s), "input file name")
            ; ("-yacc", Arg.Unit(fun ()-> options.with_yacc<- true), "exectute yacc parsing")
            ; ("-noostap", Arg.Unit (fun () -> options.with_ostap <- false), "don't execute ostap parsing")
            ]
    (fun _ -> failwith "Anonymous arguments are not supported")
    "usage msg"

let () = printf "Using input file '%s'\n%!" options.filename

(*
let () =
  let source  = read options.filename in
  let c = new OstapLexerExpr.wrap_expr_lexer source in
  let msg =
      sprintf "Ostap %s %s parsing"
        (if memoize then "memoized" else "non-mem")
        (match sort with `Good -> "left-factorized" | `Bad -> "non-factorized") in
    match (fun () -> ExprOstap3.program ~memoize sort lexer) |! eval_time msg with
      | Parsed ((xs,_),_) ->
        with_file ostap_output_file
          (fun ch -> xs |! List.map Ostap.Pretty.toString |! List.iter (fprintf ch "%s\n"));
        printf "output in %s\n"  ostap_output_file;
        if memoize then
          printf "Profit: %d\n" (!OstapLexerExpr.profit_counter);
        flush stdout
      | Failed r     ->
        printf "TT\n";
        Ostap.Reason.toString `All `Desc r |! print_endline;
        exit 1

  exit 0
        *)
(** Executing YACC printing *)
let () = if options.with_yacc then begin
  print_endline "\n============================= YACC parsing and printing ...\n";
  print_endline ("using output file " ^ yacc_output_file);
  let ch = open_in options.filename in
  let lexbuf = Lexing.from_channel ch in
  let () =
    try
      let ans = (fun () -> ExprYacc.program LexerExpr.token lexbuf) |! eval_time "YACC  parsing" in
      with_file yacc_output_file (fun ch -> ans |! List.iter (fun s -> s |! Ostap.Pretty.toString |! fprintf ch "%s\n"))
    with End_of_file -> print_endline "Some error"
  in
  close_in ch
end


(** Executing combinator printing *)
let () = if options.with_ostap then begin
  print_endline "\n============================= Ostap parsing and printing...\n";
  let source  = read options.filename in
  printf "Input length: %d\n" (String.length source);
  let doit memoize sort =
    let lexer =
      new OstapLexerExpr.t source
      (*else new OstapLexerExpr.wrap_expr_lexer source*)
    in
    let () = Ref.replace OstapLexerExpr.profit_counter (fun _ -> 0) in
    let msg =
      sprintf "Ostap %s %s parsing"
        (if memoize then "memoized" else "non-mem")
        (match sort with `Good -> "left-factorized" | `Bad -> "non-factorized") in
    match (fun () -> ExprOstap3.program ~memoize sort lexer) |! eval_time msg with
      | Parsed ((xs,_),_) ->
        with_file ostap_output_file
          (fun ch -> xs |! List.map Ostap.Pretty.toString |! List.iter (fprintf ch "%s\n"));
        printf "output in %s\n"  ostap_output_file;
        if memoize then
          printf "Profit: %d\n" (!OstapLexerExpr.profit_counter);
        flush stdout
      | Failed r     ->
        printf "TT\n";
        Ostap.Reason.toString `All `Desc r |! print_endline;
        exit 1
  in

  try
    doit true `Bad  ;
    doit true `Good ;
    doit false `Good;
    doit false `Bad
  with Stack_overflow as exn ->
    print_endline "Stack_overflow is catched!";
    Printexc.print_backtrace stdout;
    raise exn

end
