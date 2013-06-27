(** Ostap parser for small language *)
open Printf
open Ostap
open Combinators
open Helpers

type var_kind = Global | Local of int | Function
type resolver = string -> var_kind option
let is_lvalue name r = match r name with Some Global | Some (Local _) -> true | _ -> false
let is_function name r = match r name with Some Function -> true | _ -> false
let make_reason msg l = new Reason.t (Msg.make msg [||] (Matcher.Token.loc l))
let repr = Matcher.Token.repr

let rec expr ps resolver s =
  Util.expr Util.id
    [| `Lefta, [ ostap ("+"), ps#add
               ; ostap ("-"), ps#sub     ]
     ; `Lefta, [ ostap ("*"), ps#mul
               ; ostap ("/"), ps#divide  ]
    |]
    (primary ps resolver)
    s
and ostap (    (* TODO: backtracking below *)
  primary[ps][resolver]:
    fname:IDENT => { is_function (repr fname) resolver }
                :: ( make_reason (sprintf "%s should be a function" (repr fname)) fname )
                => "(" args: !(Util.list0)[expr ps resolver] ")"
                { ps#fun_call resolver (repr fname) args }

  | id:IDENT => { not (is_keyword id) && (is_lvalue (repr id) resolver) }
             :: ( make_reason "variable expected" id )
             => { ps#ident resolver (repr id) }
  | n:LITERAL   { ps#literal (Matcher.Token.repr n |! int_of_string)  }
  | -"(" expr[ps][resolver] -")"
 )

ostap (
  statement[ps][resolver]:
       l:IDENT   => { is_lvalue (repr l) resolver }
                 :: ( make_reason "global or local variable expected" l )
                 =>
          "="  r:expr[ps][resolver]             ";" { ps#assign resolver (repr l) r  }
     | "print_int" "(" e:expr[ps][resolver] ")" ";" { ps#print_int e }
     | e:expr[ps][resolver]                     ";" { ps#return_statement e }
  )

ostap (
  variable:     name:IDENT => { not (is_keyword name) } :: (make_reason  "identifier expected" name)
                           => { Matcher.Token.repr name };

  func_heading[r]: name:IDENT -"(" -")" { (Matcher.Token.repr name, [], r) }
                 | name:IDENT -"(" args: !(Util.list)[variable] -")" {
                     let newr name =
                       try Some (Local (List.mem_index_exn name args))
                       with Not_found -> r name
                     in
                     (Matcher.Token.repr name, args, newr)
                };

  func[ps][r]:   <(fname,args,r')> : func_heading[r]
                 "{" stmts:(statement[ps][r'])+ "}"
                 {
                    let ans = ps#func (fname, args, stmts) in
                    (ans, fun name -> if name=fname then Some Function else r name)
                 };

  functions[ps][r]: <(res,r1)>   : func[ps][r]
                    <(rs,rslv)>  : functions[ps][r1] { (res::rs,rslv) }
                  | $                                { ([],r) }
  ;
  global_vars[v]: "GLOBALS" xs: !(Util.list)[variable] "END" {
                       xs,(fun s -> if List.mem s xs then Some Global else None)
                  }
                | $     { [],(fun _ -> None) }
  ;
  main_prog[ps][rslv]: -"BEGIN" statement[ps][rslv]* -"END"
  ;
  program[ps]:   <(globals,rslv)> : global_vars[ps]
                 <(funcs,  rslv)> : functions[ps][rslv]
                 main:   main_prog[ps][rslv]
                 {
                   ps#program globals funcs main
                 }
)


