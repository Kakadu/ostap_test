open Ostap.Pretty
open Helpers
open Printf


(** Pretty printer in normal language*)
let printer =
  let globals : string list ref = ref [] in
  let locals  : string list ref = ref [] in
  let binop op l r  = hovboxed (listBy break [string "("; l; string op; r; string ")"]) in
  object
    method add          = binop "+"
    method mul          = binop "*"
    method sub          = binop "-"
    method divide       = binop "/"
    method arg_decl     = string

    method assign  l r  = hovboxed (listBy break [string l; string "="; r])
    method literal n    = string (string_of_int n)
    method ident        = string

    method fun_call name args =  [string name; rboxed (listByComma args)] |! seq |! hboxed

    method statement (x: printer)  = x

    method func (name,args,stmts) =
      let params = args |! List.map string |! listBy (string ",") in
      let ys =
        [hboxed (listBy break [string name; string "("; params; string ") {"])] @
        (List.map (fun x -> seq [x; string ";"]) stmts) @
        [string "}"] in
      boxed (listByNewlineBreak ys)

    method print_int e =
      hboxed (listBy break [string "print_int("; e; string ")" ])

    method program gs xs ys =
      let globals =
        match gs with
        | [] -> string ""
        | xs -> sprintf "GLOBALS %s END" (String.concat "," xs) |! string
      in
      boxed (listByNewlineBreak (globals::xs@ys) )

    method declare_locals args =
      Ref.replace locals (fun _ -> args)

    method erase_locals () =
      Ref.replace locals (fun _ -> [])

    method declare_globals xs =
      Ref.replace globals (fun _ -> xs)

  end
