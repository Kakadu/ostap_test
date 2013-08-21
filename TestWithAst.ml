open Helpers
open Printf
open Ostap.Util
open Ostap.Combinators


type options = {
  mutable filename    : string;
  mutable print       : bool;

  mutable with_yacc   : bool;
  mutable with_ostap  : bool;
  mutable with_recdesc: bool;
  mutable with_comb   : bool;
  mutable with_combOS : bool; (* my combinators andostap stream *)
  mutable with_comblexbuf : bool; (* my combinators andostap stream *)
  mutable with_recdescbuf : bool; (* recusive descent over ocamllex *)
  mutable with_recdescbuf_memoized : bool; (*  recusive descent over ocamllex with lexeme memoization *)
  mutable with_recdescbuf_memoized2: bool; (*  recusive descent over ocamllex with lexeme memoization and using it *)
}
(*
let () =
  Gc.(
    let c = get () in
    c.minor_heap_size <- 536870912*2;
    set c
  )
  *)

let options = {
  filename="expr2.e"; with_yacc=false;       with_ostap=false;      with_recdesc=false; with_comb=false;
  with_combOS=false;  with_comblexbuf=false; with_recdescbuf=false; with_recdescbuf_memoized=false;
  with_recdescbuf_memoized2=false;
  print=false;
}

let yacc_output_file  =     "yacc.expr.out"
let hack_output_file  =     "comb.expr.out"
let ostap_output_file  =    "ostap.expr.out"
let recdesc_output_file =   "recdesc.expr.out"
let combOS_output_file  =   "combOS.expr.out"
let comblexbuf_output_file = "comblexbuf.expr.out"
let recdescbuf_output_file = "recdescbuf.expr.out"
let recdescbuf_memoized_output_file = "recdescbuf.mem.expr.out"
let recdescbuf_memoized2_output_file = "recdescbuf.mem2.expr.out"

let () =
  Arg.parse [ ("-f",       Arg.String (fun  s -> options.filename <- s), "input file name")
            ; ("-print",   Arg.Unit   (fun () -> options.print    <- true), "print evaluated AST")
            ; ("-yacc",    Arg.Unit   (fun () -> options.with_yacc<- true), "execute yacc/menhir parsing")
            ; ("-comb",    Arg.Unit   (fun () -> options.with_comb       <- true), "execute combintor parsing")
            ; ("-combOS",  Arg.Unit   (fun () -> options.with_combOS  <- true), "execute combintor parsing on ostap stream")
            ; ("-recdesc", Arg.Unit   (fun () -> options.with_recdesc <- true), "execute resdesc parsing")
            ; ("-ostap",   Arg.Unit   (fun () -> options.with_ostap     <- true), "execute ostap parsing")
            ; ("-comblexbuf", Arg.Unit(fun () -> options.with_comblexbuf<- true),
               "execute combinators with lexbuf stream parsing")
            ; ("-recdescbuf", Arg.Unit(fun () -> options.with_recdescbuf<- true),
               "execute recursive descent over lexbuf")
            ; ("-recdescbuf-mem", Arg.Unit(fun () -> options.with_recdescbuf_memoized <- true),
               "execute recursive descent over lexbuf with lexeme memoization")
            ; ("-recdescbuf-mem2", Arg.Unit(fun () -> options.with_recdescbuf_memoized2 <- true),
               "execute recursive descent over lexbuf with lexeme memoization and using this memoization")
            ]
    (fun _ -> failwith "Anonymous arguments are not supported")
    "usage msg"

let () = printf "Using input file '%s'\n%!" options.filename

let clear_caches () = () (*
  print_endline "clearing caches";
  Sys.command "sync && sudo bash -c \"echo 3 > /proc/sys/vm/drop_caches\"" |! ignore *)

(** Executing YACC printing *)
let run_lr () = if options.with_yacc then begin
  clear_caches ();
  print_endline "============================= YACC parsing and printing ...\n";
  let ch = open_in options.filename in

  let () =
    try
      let xs = (fun () ->
        let lexbuf = Lexing.from_channel ch in
        ExprYaccAst.program LexerExprAst.token lexbuf) |! eval_time "YACC  parsing"
      in
      Gc.compact ();
      if options.print then begin
        print_endline ("using output file " ^ yacc_output_file);
        let ps = (fun () -> xs |! List.map (fun x -> x |! Ast.print |! Ostap.Pretty.toString))
                 |! eval_time "ast -> printer" in
        with_file yacc_output_file
          (fun ch -> ps |! List.iter (fprintf ch "%s\n"))
      end
    with End_of_file -> print_endline "Some error"
  in
  close_in ch
end

open HackParserLiftingAst
let run_comb () = if options.with_comb then begin
    clear_caches ();
    print_endline "============================= My comb-s with ocamllex stream parsing and printing...\n";
    let source  = read options.filename in
    printf "Input length: %d\n" (String.length source);

    let do_parse, lexer_time = program source  in
    Gc.compact ();
    let (ans, parser_time)    = eval_time_2 do_parse in
    Gc.compact ();
    match ans with
      | Comb.Parsed(xs,_) ->
        printf "action 'My combs' parsing tooks %f time (%f lexer + %f parsing)\n%!"
          (lexer_time +. parser_time) lexer_time parser_time;
        let ps = (fun () -> xs |! List.map (fun x -> x |! Ast.print |! Ostap.Pretty.toString))
                |! eval_time "ast -> printer" in
        with_file hack_output_file
          (fun ch -> ps |! List.iter (fprintf ch "%s\n"));
        printf "output in %s\n"  hack_output_file;
        flush stdout
      | Comb.Failed ->
        printf "TT\n";
        exit 1
    end


open HackParserLiftingAstOL
let run_combOS () = if options.with_comb then begin
    clear_caches ();
    print_endline "============================= My comb-s with ocamllex stream parsing and printing...\n";
    let source  = read options.filename in
    printf "Input length: %d\n" (String.length source);

    let do_parse = program source  in
    Gc.compact ();
    let (ans, parser_time)    = eval_time_2 do_parse in
    Gc.compact ();
    match ans with
      | Comb.Parsed(xs,_) ->
        printf "action 'My combs' parsing tooks %f time\n%!"
          parser_time;
        let ps = (fun () -> xs |! List.map (fun x -> x |! Ast.print |! Ostap.Pretty.toString))
                |! eval_time "ast -> printer" in
        with_file hack_output_file
          (fun ch -> ps |! List.iter (fprintf ch "%s\n"));
        printf "output in %s\n"  hack_output_file;
        flush stdout
      | Comb.Failed ->
        printf "TT\n";
        exit 1
    end

open HackParserLiftingAstLexbuf
let run_comblexbuf () = if options.with_comblexbuf then begin
    clear_caches ();
    print_endline "============================= My comb on lexbuf stream parsing and printing...\n";
    let source  = read options.filename in
    printf "Input length: %d\n" (String.length source);

    let do_parse = program source  in
    let (ans, parser_time)  = eval_time_2 do_parse in
    Gc.compact ();
    match ans with
      | Comb.Parsed(xs,_) ->
        printf "action 'combs over lexbuf' parsing tooks %f time\n%!" parser_time;
        let ps = (fun () -> xs |! List.map (fun x -> x |! Ast.print |! Ostap.Pretty.toString))
                |! eval_time "ast -> printer" in
        with_file comblexbuf_output_file (fun ch -> ps |! List.iter (fprintf ch "%s\n"));
        printf "output in %s\n" comblexbuf_output_file;
        flush stdout
      | Comb.Failed ->
        printf "TT\n";
        exit 1
    end

open RecDescAst
let run_recdesc () = if options.with_recdesc then begin
    clear_caches ();
    print_endline "\n============================= Recursive Descent and printing...\n";
    let source  = read options.filename in
    printf "Input length: %d\n" (String.length source);
    printf "output in %s\n"  recdesc_output_file;

    let do_parse, lexer_time = program source  in
    let (ans, parser_time)    = eval_time_2 do_parse in

    with_file recdesc_output_file
      (fun ch -> ans |! List.map (fun x -> x |! Ast.print |! Ostap.Pretty.toString) |! List.iter (fprintf ch "%s\n"));

    printf "action 'rec desc parsing' tooks %f time (%f lexer + %f parsing)\n%!"
      (lexer_time +. parser_time) lexer_time parser_time;
    flush stdout
end

open RecDescAstOverLexbuf
let run_recdescbuf () = if options.with_recdescbuf then begin
    clear_caches ();
    print_endline "============================= Recursive Descent over lexbuf parsing and printing...\n";
    let source  = read options.filename in
    printf "Input length: %d\n" (String.length source);
    printf "output in %s\n"  recdescbuf_output_file;

    let do_parse  = program source  in
    let (ans, parser_time)    = eval_time_2 do_parse in
    printf "action 'rec desc parsing over lexbuf' tooks %f time\n%!" parser_time;
    if options.print then begin
      Gc.compact ();
      let ps = (fun () -> ans |! List.map (fun x -> x |! Ast.print |! Ostap.Pretty.toString))
               |! eval_time "ast -> printer" in
      with_file recdescbuf_output_file (fun ch -> ps |! List.iter (fprintf ch "%s\n"));
    end;
    flush stdout
end

open RecDescAstOverLexbufMemoized
let run_recdescbuf_memoized () = if options.with_recdescbuf_memoized then begin
    clear_caches ();
    print_endline "=========================== Recursive Descent over lexbuf (with memoization) parsing and printing...\n";
    let source  = read options.filename in
    printf "Input length: %d\n" (String.length source);
    printf "output in %s\n"  recdescbuf_memoized_output_file;

    let do_parse  = program source  in
    let (ans, parser_time)    = eval_time_2 do_parse in
    printf "action 'rec desc parsing over lexbuf (with mem)' tooks %f time\n%!" parser_time;
    if options.print then begin
      Gc.compact ();
      let ps = (fun () -> ans |! List.map (fun x -> x |! Ast.print |! Ostap.Pretty.toString))
               |! eval_time "ast -> printer" in
      with_file recdescbuf_memoized_output_file (fun ch -> ps |! List.iter (fprintf ch "%s\n"));
    end;
    flush stdout
end

open RecDescAstOverLexbufMemoized2
let run_recdescbuf_memoized2 () = if options.with_recdescbuf_memoized2 then begin
    clear_caches ();
    print_endline "=========================== Recursive Descent over lexbuf (with memoization) parsing and printing...\n";
    let source  = read options.filename in
    printf "Input length: %d\n" (String.length source);
    printf "output in %s\n"  recdescbuf_memoized2_output_file;

    let do_parse  = program source  in
    let (ans, parser_time)    = eval_time_2 do_parse in
    printf "action 'rec desc parsing over lexbuf (with mem)' tooks %f time\n%!" parser_time;
    if options.print then begin
      Gc.compact ();
      let ps = (fun () -> ans |! List.map (fun x -> x |! Ast.print |! Ostap.Pretty.toString))
               |! eval_time "ast -> printer" in
      with_file recdescbuf_memoized2_output_file (fun ch -> ps |! List.iter (fprintf ch "%s\n"));
    end;
    flush stdout
end

let run_ostap () = if options.with_ostap then begin
    clear_caches ();

    print_endline "\n============================= Ostap and printing...\n";
    let source  = read options.filename in
    printf "Input length: %d\n" (String.length source);
    printf "output in %s\n%!"  ostap_output_file;

    let (ans, parser_time)    = eval_time_2 (fun () -> ExprOstapAst.program (new OstapLexerExpr.t source)) in
    match ans with
      | Parsed((xs,_), _) ->
        printf "action 'ostap parsing' tooks %f time \n%!" parser_time;
        with_file ostap_output_file
          (fun ch -> xs |! List.map (fun x -> x |! Ast.print |! Ostap.Pretty.toString) |! List.iter (fprintf ch "%s\n"));
        flush stdout
      | Failed r ->
        printf "TT\n";
        Ostap.Reason.toString `All `Desc r |! print_endline;
        exit 1
    end

let () =
  run_lr ();
  run_comb ();
  run_recdesc ();
  run_ostap ();
  run_combOS ();
  run_comblexbuf ();
  run_recdescbuf ();
  run_recdescbuf_memoized ();
  run_recdescbuf_memoized2 ()

