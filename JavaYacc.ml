(** Pretty printer of assembler *)
open Ostap.Pretty
open Printf
open Lang
open Helpers

open JavaOstap

(* instead of passing resolver we need to store *)
let printer  =
  let globals   : string list ref = ref [] in
  let locals    : string list ref = ref [] in
  let functions : string list ref = ref [] in
  let resolver name =
    if List.mem name !globals then Some Global
    else if List.mem name !functions then Some Function
    else Option.map (fun n -> Local n) (List.mem_index name !locals)
  in
  let binop op l r  = vboxed (listBy break [l; r; seq [string "  "; string op]]) in

  object (self)
    method declare_globals xs =
      (*printf "declare globals %s\n%!" (String.concat "," xs); *)
      Ref.replace globals (fun _ -> xs)
    method declare_locals  xs =
      (*printf "declare locals %s\n%!" (String.concat "," xs);*)
      Ref.replace locals  (fun _ -> xs)
    method erase_locals ()    =
      (*printf "erase locals\n%!";*)
      Ref.replace locals  (fun _ -> [])

    method add         = binop "iadd"
    method mul         = binop "imul"
    method sub         = binop "isub"
    method divide      = binop "idiv"
    method arg_decl    = string

    method assign l r =
      let l =
        match resolver l with
          | Some Global    -> sprintf "  putstatic  HelloWorld/%s I" l
          | Some (Local n) -> sprintf "  istore %d" n
          | _ -> failwith (sprintf "can't resolve variable %s" l)
      in
      [r; string l] |! listBy break |! vboxed

    method literal n     = string (sprintf "  bipush %d" n)

    method ident  name    =
      match resolver name with
        | Some Global    -> sprintf "  getstatic HelloWorld/%s I" name |! string
        | Some (Local n) -> sprintf "  iload_%d ; it's an argument `%s`" n name |! string
        | _ -> failwith (sprintf "ident '%s' should be a global or local variable" name)

    method fun_call name args =
      [ args |! listBy break |! vboxed;
        seq [string "  invokestatic "; string (java_meth_sig ~name args)]
      ] |! listBy break |! vboxed

    method statement (x: printer)  = x
    method func (name,args,xs) =
      let (_:string list) = args in
      let a = [ string ".method public static "; string name
              ; string "("; String.concat "" (List.map (fun _ -> "I") args) |! string; string ")I"
              ] |! seq |! hboxed in
      let b = vboxed (listBy break xs)  in
      [a;
       string "  .limit stack 100";
       string "  .limit locals 100";
       b;
       string "  ireturn";
       string(".end method")
      ] |! listBy break |! vboxed

    method program gs xs ys =
      let globals =
        match gs with
        | [] -> [string "; no any global variables/fields"]
        | __ ->
            gs |! List.map (fun name -> sprintf ".field public static %s I" name |! string)
      in
      let main_header = [string "";
                         string ".method public static main([Ljava/lang/String;)V";
                         string "  .limit stack 100" ] in
      ((javaHeader globals)::xs@main_header @ys@ [string "  return"; string ".end method\n"])
        |! listBy break  |! vboxed

    method print_int e =
      [e; string "  invokestatic HelloWorld/printInt(I)V"] |! listByNewlineBreak |! boxed

end
