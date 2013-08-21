type expr =
  | Ident of string
  | Literal of int
  | Call of string * expr list
  | Sum of expr * expr
  | Mul of expr * expr

let ps = OstapExprPrinter.printer
let rec print = function
  | Ident x -> ps#ident x
  | Literal x -> ps#literal x
  | Call(name,args) -> ps#fun_call name (List.map print args)
  | Sum (x,y) -> ps#add (print x) (print y)
  | Mul (x,y) -> ps#mul (print x) (print y)
