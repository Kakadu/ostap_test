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

class ['ans] t s =
  let skip    = Skip.create [Skip.whitespaces " \n\t\r"; Skip.nestedComment "(*" "*)"] in
  let ident   = regexp identRegexp in
  let literal = regexp "[0-9]+" in

  object (self)
    inherit Matcher.t s
    val h = MemoMap0.create 100
    method skip p coord = skip s p coord

    method getIDENT     = self#get "identifier" ident
    method getLITERAL   = self#get "literal"  literal

    method memoize0 (p: 'ans t -> 'ans): 'ans t -> 'ans =
      (fun (s: 'ans t) ->
        let k : MemoMap0.key = (Obj.magic p, s#pos) in
        try (*printf "hashtbl size = %d\n" (MemoMap0.length h);
            printf "%d find\n" (HashKey0.hash k); *)
            let ans = MemoMap0.find h k in
            (*printf "found\n";*)
            incr profit_counter;
            Obj.magic ans
        with Not_found ->
          let ans = p s in
          let ans': unit = Obj.magic ans in

          let () = MemoMap0.add h k ans' in
          (*printf "%d add\n" (HashKey0.hash k);*)
          Obj.magic ans
      )

  end

