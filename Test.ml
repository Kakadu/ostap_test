(** Source code generation *)
open Printf

let with_file name f =
  let ch = open_out name in
  f ch ;
  close_out ch

let pair a b : string =
  sprintf
    "%s + f0(%s,4*%s) * f0(5,%s - f0(f0(%s,%s),f0(2,(%s) - (%s * %s * %s)))) / f0(%s,f0(%s,f0(%s,f0(%s,%s))))"
     a       b     a         b          b  a         a       b    a    a         a      b    a     b   a

let make n ch =
  fprintf ch "f0(a,b) { a+b; }\n";
  let rec f i acc1 acc2 =
    fprintf ch "f%d() { %s+%s; }\n" i acc1 acc2;
    if n < i then fprintf ch "BEGIN\n print_int(f%d());\nEND\n" i
    else f (i+1) (pair acc1 acc2) (pair acc2 acc1)
  in
  f 1 "1" "2"
