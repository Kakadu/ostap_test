open Ostap.Pretty
open Helpers
open Printf

(** Pretty printer in normal language*)

type counters = {
  mutable add: int;
  mutable mul: int;
  mutable sub: int;
  mutable divide: int;
  mutable arg_decl: int;
  mutable literal: int;
  mutable ident: int;
  mutable fun_call: int;
  mutable statement: int;
}

let counter = { add=0; mul=0; sub=0; divide=0; arg_decl=0; literal=0; ident=0; fun_call=0; statement=0; }
let clear_counter () =
  counter.add <- 0;
  counter.mul <- 0;
  counter.sub <- 0;
  counter.divide <- 0;
  counter.arg_decl <- 0;
  counter.literal <- 0;
  counter.ident <- 0;
  counter.fun_call <- 0;
  counter.statement <- 0

let print_counter () =
  printf "add=%d, mul=%d, sub=%d, divide=%d, arg_decl=%d\n"
    counter.add counter.mul counter.sub counter.divide counter.arg_decl;
  printf "literal=%d, ident=%d, fun_call=%d, statement=%d\n%!"
    counter.literal counter.ident counter.fun_call counter.statement

let printer =
  let binop op l r  = hovboxed (listBy break [string "("; l; string op; r; string ")"]) in
  object
    method add x y       =
      counter.add <- counter.add + 1;
      binop "+" x y
    method mul x y       =
      counter.mul <- counter.mul + 1;
      binop "*" x y
    method sub     x y   =
      counter.sub <- counter.sub + 1;
      binop "-" x y
    method divide x y    =
      counter.divide <- counter.divide + 1;
      binop "/" x y
    method arg_decl x     =
      counter.arg_decl <- counter.arg_decl + 1;
      string x

    method literal n     =
      counter.literal <- counter.literal + 1;
      string (string_of_int n)
    method ident x        =
      counter.ident <- counter.ident + 1;
      string x
    method fun_call   name args =
      counter.fun_call <- counter.fun_call + 1;

      [string name; rboxed (listByComma args)] |! seq |! hboxed
    method statement s =
      counter.statement <- counter.statement + 1;
      s
  end
