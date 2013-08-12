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
    c.minor_heap_size <- 536870912*2;
    c.max_overhead <-      1000000;
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

let hack_output_file = "hack.expr.out"

(** Executing combinator printing *)
let () = if options.with_ostap then begin
    print_endline "\n============================= Hack parsing and printing...\n";
    let source  = read options.filename in
    printf "Input length: %d\n" (String.length source);

    let do_parse, lexer_time =  HackParser.program source  in
    let ans, parser_time    = eval_time_2 do_parse in
    match ans with
      | HackParser.Comb.Parsed (xs,_) ->
        with_file hack_output_file
          (fun ch -> xs |! List.map Ostap.Pretty.toString |! List.iter (fprintf ch "%s\n"));
        printf "output in %s\n"  hack_output_file;
        printf "action hack parsing tooks %f time (%f lexer + %f parsing)\n%!"
          (lexer_time +. parser_time) lexer_time parser_time;
        flush stdout
      | HackParser.Comb.Failed     ->
        printf "TT\n";
        exit 1

end
