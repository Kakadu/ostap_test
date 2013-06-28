open Ostap
open String
open Printf
open Str
open Ostap.Matcher

let identRegexp = "[a-zA-Z]\([a-zA-Z0-9]\)*\\b"

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

module HashKey  = struct
    type t = unit * unit * int
    let equal (a,b,c) (q,w,e) = c=e && a==q && b==w
    let hash (a,b,c) =
      let ans = Hashtbl.hash a + (Hashtbl.hash b + c*17407)*65599 in
      (*printf "hash = %d\n%!" ans;*)
      ans
end;;

module MemoMap1 = Hashtbl.Make(HashKey);;

let () =
  let k1 = (Obj.magic ((+)1),  Obj.magic ((^)""), 18) in
  let k2 = (Obj.magic ((^)""), Obj.magic ((+)1) , 20) in
  let h = MemoMap1.create 10 in
  MemoMap1.add h k1 ();
  MemoMap1.add h k2 ();
  ignore (MemoMap1.find h k1);
  ignore (MemoMap1.find h k2)
;;
let profit_counter = ref 0

class ['ans, 'arg] t s =
  let skip    = Skip.create [Skip.whitespaces " \n\t\r"; Skip.nestedComment "(*" "*)"] in
  let ident   = regexp identRegexp in

  object (self)
    inherit Matcher.t s
    val h = MemoMap1.create 100
    method skip p coord = skip s p coord

    method getIDENT     = self#get "identifier" ident

(*
    method memoize1 (p: 'arg -> ('ans, 'arg) t -> 'ans) : 'arg -> ('ans,'arg) #t -> 'ans =
      (*let h = MemoMap1.create 100 in*)
      (fun arg (s: ('ans,'arg) #t) ->
        let k : MemoMap1.key = (Obj.magic p, Obj.magic arg, s#pos) in
        try (*printf "hashtbl size = %d\n" (MemoMap1.length h);*)
            printf "%d find\n" (HashKey.hash k);
            let ans = MemoMap1.find h k in
            printf "found\n";
            incr profit_counter;
            Obj.magic ans
        with Not_found ->
          let ans = p arg s in
          let ans': unit = Obj.magic ans in

          let () = MemoMap1.add h k ans' in
          printf "%d add\n" (HashKey.hash k);
          Obj.magic ans
      )
*)
  end

