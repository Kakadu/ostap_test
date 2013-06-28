open Helpers
open Printf
open Ostap.Util
open Ostap.Combinators


type options = {
  mutable filename  : string;
  mutable with_yacc : bool;
  mutable with_ostap: bool;
}
let () =
  Gc.(
    let c = get () in
    c.minor_heap_size <- 26214400;
    (*printf "minor_heap_size = %d\n%!" Gc.(c.minor_heap_size);*)
    c.max_overhead <- 1000000;
    (*printf "max_overhead    = %d\n%!" Gc.(c.max_overhead);*)
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

(** Executing YACC printing *)
let () = if options.with_yacc then begin
  print_endline "\n============================= YACC parsing and printing ...\n";
  let ch = open_in options.filename in
  let lexbuf = Lexing.from_channel ch in
  let () =
    try
      let ans = (fun () -> ExprYacc.program LexerExpr.token lexbuf) |! eval_time "YACC  parsing" in
      with_file "yacc.expr.out" (fun ch -> ans |! List.iter (fun s -> s |! Ostap.Pretty.toString |! fprintf ch "%s\n"))
    with End_of_file -> print_endline "WTF"
  in
  close_in ch
end


(** Executing combinator printing *)
let () = if options.with_ostap then begin
  print_endline "\n============================= Ostap parsing and printing...\n";
  let source  = read options.filename in
  let lexer = new OstapLexerExpr.t source in
  let () = Ref.replace OstapLexerExpr.profit_counter (fun _ -> 0) in
  match (fun () -> ExprOstap.program lexer) |! eval_time "Ostap parsing" with
  | Parsed ((xs,_),_) ->
    with_file "ostap.expr.out"
      (fun ch -> xs |! List.map Ostap.Pretty.toString |! List.iter (fprintf ch "%s\n"));
    print_string "Profit: ";
    print_int (!OstapLexerExpr.profit_counter);
    print_newline ()


  | Failed r     ->
      printf "TT\n";
      Ostap.Reason.toString `All `Desc r |! print_endline;
      exit 1
end
