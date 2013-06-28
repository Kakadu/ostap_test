open Ostap.Pretty
open Helpers
open Printf

(** Pretty printer in normal language*)
(*
let printer =
  let binop op l r  = hovboxed (listBy break [string "("; l; string op; r; string ")"]) in
  object
    method add           = binop "+"
    method mul           = binop "*"
    method sub           = binop "-"
    method divide        = binop "/"
    method arg_decl      = string

    method literal n     = string (string_of_int n)
    method ident         = string
    method fun_call   name args =  [string name; rboxed (listByComma args)] |! seq |! hboxed

  end
  *)
let printer =
  let binop op l r  = hovboxed (listBy break [string "("; l; string op; r; string ")"]) in
  object
    method add           = binop "+"
    method mul           = binop "*"
    method sub           = binop "-"
    method divide        = binop "/"
    method arg_decl      = string

    method literal n     = string (string_of_int n)
    method ident         = string
    method fun_call   name args =  [string name; rboxed (listByComma args)] |! seq |! hboxed
    method statement s = s
  end
