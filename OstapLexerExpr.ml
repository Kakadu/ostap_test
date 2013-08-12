open Ostap
open String
open Printf
open Str
open Ostap.Matcher

let identRegexp = "[a-zA-Z]\([a-zA-Z0-9]\)*\\b"
(*
let gen_memorizer c len =
  let arr = Array.create len false in
  let has k = arr.(k) in
  let data = Array.create len (Combinators.Failed None) in
  let get pos = incr c; data.(pos) in
  let add pos v =
    (*print_endline "XFCE"; *)
    arr .(pos) <- true;
    data.(pos) <- v
  in
  (has,get,add)
  *)
module HashKey0 = struct
    type t = unit * int
    let equal (a,c) (q,e) = c=e && a==q
    let hash (a,c) =
       Hashtbl.hash a + c*17407
end;;

module MemoMap0 = Hashtbl.Make(HashKey0)

let profit_counter = ref 0

exception Finished
exception Integer of int

class ['ans] wrap_expr_lexer stream =
  let buf = Lexing.from_string stream in
  let data = Array.init (String.length stream) (fun _ -> None) in
  let _print i d = match d with Some (_,r,s) -> printf "%d:     '%s'      %d\n" i s r | None -> () in
  let () =
    try
      while true do
        let ans =  LexerExpr.token buf in
        let start = Lexing.lexeme_start buf in
        (*printf "start = %d\n%!" start;*)
        let r = Lexing.lexeme_end buf in
        let v = Some (ans, r, Lexing.lexeme buf) in
        (*print start v;*)
        if start=r then raise Finished;
        data.(start) <- v
      done;
      assert false
    with Finished -> (print_endline "Lexbuf lexing finished")
  in
  object(self)
    inherit Matcher.t stream
    val pos = 0
    method  get_pos = pos
    method private parsed x y c = Ostap.Combinators.Parsed (((x, c), y), None)
    method private failed x c   = Ostap.Combinators.Failed (Ostap.Reason.reason (Msg.make x [||] (Msg.Locator.Point c)))

    method getIDENT =
      (*
      try for i = 1 to Array.length data - 1 do if data.(i) <> None then raise (Integer i) done;
          self#failed "IDENT expected but stream is empty" (-1,-1)
      with Integer n ->
        match data.(n) with
          | None -> assert false
          | Some (ExprYacc.IDENT s,r,_) -> self#parsed s {< pos = r >} (-1,-1)
          | ___ -> self#failed "IDENT expected" (-1,-1) *)

      match data.(pos) with
        | Some (ExprYacc.IDENT s,r,_) -> self#parsed s {< pos = r >} (-1,-1)
        | ___ -> self#failed "IDENT expected" (-1,-1)

    method getLITERAL =
      (*print_endline "getLiteral";*)
      match data.(pos) with
        | Some (ExprYacc.LITERAL _,r,s) -> self#parsed s {< pos = r >} (-1,-1)
        | ___ -> self#failed "LITERAL expected" (-1,-1)

    method look s =
      match data.(pos) with
        | Some (_,r,s') when s' = s ->  self#parsed s {< pos = r >} (-1,-1)
        | ________________________  -> self#failed (sprintf "%s expected" s) (-1,-1)

    val h = MemoMap0.create 100
    method memoize0 (name: string) (p: 'ans wrap_expr_lexer -> 'ans): 'ans wrap_expr_lexer -> 'ans =
      (fun (s: 'ans wrap_expr_lexer) ->
        let k : MemoMap0.key = (Obj.magic p, s#get_pos) in
        try (*printf "hashtbl size = %d\n" (MemoMap0.length h);
            printf "trying to find %d when pos=%d and parser=%d\n" (HashKey0.hash k) s#pos (Hashtbl.hash p); *)
            let ans = MemoMap0.find h k in (*
            printf "found\n";*)
            incr profit_counter;
            Obj.magic ans
        with Not_found ->
          let ans = p s in
          let ans': unit = Obj.magic ans in

          let () = MemoMap0.add h k ans' in
          (*printf "%d add\n" (HashKey0.hash k); *)
          Obj.magic ans
      )
  end

class ['ans] t s =
  let skip    = Skip.create [Skip.whitespaces " \n\t\r"; Skip.nestedComment "(*" "*)"] in
  let ident   = regexp identRegexp in
  let literal = regexp "[0-9]+" in

  object (self)
    inherit Matcher.t s
    val h = MemoMap0.create 100
    method skip p coord  = skip s p coord

    method getIDENT : (Token.t * _, Reason.t) Combinators.tag  = self#get "identifier" ident
    method getLITERAL   = self#get "literal"  literal

    method memoize0 (name: string) (p: 'ans t -> 'ans): 'ans t -> 'ans =
      (fun (s: 'ans t) -> (*
        printf "executing memoized version of %s at pos %d\n" name s#pos;*)
        let k : MemoMap0.key = (Obj.magic p, s#pos) in
        try (*printf "hashtbl size = %d\n" (MemoMap0.length h);
            printf "trying to find %d when pos=%d and parser=%d\n" (HashKey0.hash k) s#pos (Hashtbl.hash p); *)
            let ans = MemoMap0.find h k in (*
            printf "found\n";*)
            incr profit_counter;
            Obj.magic ans
        with Not_found ->
          let ans = p s in
          let ans': unit = Obj.magic ans in

          let () = MemoMap0.add h k ans' in
          (*printf "%d add\n" (HashKey0.hash k); *)
          Obj.magic ans
      )

  end

