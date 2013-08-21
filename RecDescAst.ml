open Helpers
open Printf

let data : (ExprYaccAst.token * int * int * string) array ref = ref [| |]
let data_len = ref (-1)

let scanner = ref (-1)
let nextToken () = incr scanner
let curToken () = !data.(!scanner)
let finished () = !scanner >= !data_len

let rec arguments acc =
  let l = expr () in
  match curToken () with
    | (ExprYaccAst.COMMA,_,_,_) -> nextToken (); arguments (l::acc)
    | _ -> List.rev (l::acc)

and primary () =
  match !data.(!scanner) with
    | (ExprYaccAst.LITERAL _,_,_,s) ->
         nextToken ();(*
      print_endline "literal parsed"; *)
         Ast.Literal (int_of_string s)
    | (ExprYaccAst.LPAREN,_,_,_)    -> begin
          let pos = !data_len in
          nextToken ();
          let e = expr () in
          match curToken () with
            | (ExprYaccAst.RPAREN,_,_,_) -> nextToken (); e
            | _____ -> failwith "Missing ')'"
    end
    | (ExprYaccAst.IDENT id,_,_,_) when !scanner = !data_len - 1 -> Ast.Ident id
    | (ExprYaccAst.IDENT id,_,_,_)  -> begin (* function call *)
         match !data.(!scanner + 1) with
           | (ExprYaccAst.LPAREN,_,_,_) ->
             nextToken (); nextToken (); (*
             let () = printf "before `arguments` curpos = %d\n%!" !scanner in *)
             let args = arguments [] in (*
             let () = printf "arguments finished. curpos = %d\n%!" !scanner in *)
             begin
             match curToken () with
               | (ExprYaccAst.RPAREN,_,_,_) -> nextToken(); Ast.Call (id, args)
               | _ -> failwith (sprintf "Missing ')' at pos %d" !scanner)
             end
           | _ -> Ast.Ident id
    end
    | _ -> failwith (sprintf "missing primary expression at pos %d" !scanner)

and factor () =
  let l = primary () in
  if finished () then l
  else
  match curToken () with
    | (ExprYaccAst.TIMES,_,_,_) ->
      begin
        nextToken ();
        if finished () then failwith "factor expected" else
          let r = factor () in
          Ast.Mul (l, r)
      end
    | ___ -> l

and expr () =
  let l = factor () in
  if finished () then l
  else (*
    let () = print_endline "checking for +" in *)
    match curToken () with
      | (ExprYaccAst.PLUS,_,_,_) ->
        begin
          nextToken ();
          let r = expr () in
          Ast.Sum (l,r)
        end
      | ___ -> l

let statement () =
  let e = expr () in
  if finished () then failwith "';' expected" else nextToken ();
  e

let main () =
  scanner := 0;
  try
    let ans = statement () in
    assert (!scanner = !data_len);
    [ans]
  with exc ->
    Printexc.to_string exc |! print_endline;
    raise exc

let program stream =
  let data_ = Array.init (String.length stream) (fun _ -> None) in
  let buf = Lexing.from_string stream in
  let count = ref 0 in
  let foo () =
    try
      while true do
        let ans =  LexerExprAst.token buf in
        let start = Lexing.lexeme_start buf in
        let r = Lexing.lexeme_end buf in
        let v = Some (ans, start, r, Lexing.lexeme buf) in
        if start=r then raise OstapLexerExpr.Finished;
        incr count;
        data_.(start) <- v
      done;
      assert false
    with OstapLexerExpr.Finished -> ()
  in
  let (_,lexer_time) = Helpers.eval_time_2 foo in

  let () =
    let data' = Array.init !count (fun _ -> (ExprYaccAst.EOF,-1,-1,"") ) in
    let u = ref 0 in
    Array.iter (fun item -> match item with Some p -> (data'.(!u) <- p; incr u) | None -> ()) data_;
    assert (!u = !count);
    data_len := Array.length data';

    data := data'
  in

  (main,lexer_time)



