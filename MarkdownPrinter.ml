open Ostap.Pretty
open Helpers
open Printf


(** Pretty printer in normal language*)
let printer =
  object
    method text   s      = seq [string "text ("; string s; string ")"; break]
    method italic xs     = seq [ seq ((string "<i>") :: xs);  string "</i>"; break]
    method bold   xs     = seq [ seq ((string "<b>") :: xs);  string "</b>"; break]
    method seq   xs ys   = seq [ seq xs; seq ys ]
  end
