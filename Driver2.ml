open Helpers
open Printf
open Ostap.Util
open Ostap.Combinators


type options = {
  mutable filename  : string;
  mutable with_yacc : bool;
  mutable with_ostap: bool;
  mutable with_java : bool;
}

let options = { filename="input.xxx"; with_yacc=false; with_ostap=true; with_java=false }

let () =
  Arg.parse [ ("-f", Arg.String (fun s -> options.filename <- s), "input file name")
            ; ("-yacc", Arg.Unit(fun ()-> options.with_yacc<- true), "exectute yacc parsing")
            ; ("-java", Arg.Unit(fun ()-> options.with_java<- true), "generate java bytecode")
            ; ("-noostap", Arg.Unit (fun () -> options.with_ostap <- false), "don't execute ostap parsing")
            ]
    (fun _ -> failwith "Anonymous arguments are not supported")
    "usage msg"

let () = printf "Using input file '%s'\n%!" options.filename

(*
(** Warmup *)
let () =
  (fun () -> Lang.program Printer.printer (new OstapLexer.t source)) |! ignore
*)

(** Executing YACC printing *)
let () = if options.with_yacc then begin
  print_endline "\n============================= YACC parsing and printing ...\n";
  let ch = open_in options.filename in
  let lexbuf = Lexing.from_channel ch in
  let () =
    try
      let ans = (fun () -> Parser.main Lexer.token lexbuf) |! eval_time "YACC parsing" in
      with_file "yacc.out" (fun ch -> ans |! Ostap.Pretty.toString |! fprintf ch "%s\n")
    with End_of_file -> print_endline "WTF"
  in
  close_in ch
end
(*
(* YACC printer to Java *)
let () = if options.with_java && options.with_yacc then begin
  print_endline "\n============================= YACC java generation...\n";
  let ch = open_in options.filename in
  let lexbuf = Lexing.from_channel ch in
  let () =
    try
      Parser.set_printer JavaYacc.printer;
      let ans = (fun () -> Parser.main Lexer.token lexbuf)  |! eval_time "YACC parsing" in
      with_file "HelloWorldYacc.j" (fun ch -> ans |! Ostap.Pretty.toString |! fprintf ch "%s\n")
    with End_of_file -> print_endline "WTF"
  in
  close_in ch
end
    *)
(** Executing combinator printing *)
let () = if options.with_ostap then begin
  print_endline "\n============================= Ostap parsing and printing...\n";
  let source  = read options.filename in
  let lexer = new OstapLexer.t source in
  match (fun () -> Lang2.program lexer) |! eval_time "Ostap parsing" with
  | Parsed ((xs,_),_) ->
    printf "Lookups to table: %d\n%!" !OstapLexer.profit_counter;
    with_file "ostap.out" (fun ch -> Ostap.Pretty.toString xs |! fprintf ch "%s\n")
  | Failed r     ->
      printf "TT\n";
      Ostap.Reason.toString `All `Desc r |! print_endline;
      exit 1
end
(*
open Ostap.Pretty
open Helpers

(* Ostap printer to Java *)
let () = if options.with_java && options.with_ostap then begin
  let out_filename = "HelloWorld.j" in
  let source  = read options.filename in
  print_endline "\n============================= Ostap java generation ...\n";
  match (fun () -> Lang.program JavaOstap.toJava (new OstapLexer.t source))|!eval_time "with Ostap to Java" with
    | Parsed ((xs,_),_) ->
        with_file out_filename (fun ch -> toString xs |! (fprintf ch "%s\n") )
    | Failed r       ->
        Ostap.Reason.toString `All `Desc r |! print_endline;
        exit 1
end
    *)
