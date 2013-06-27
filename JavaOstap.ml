(** Pretty printer of assembler *)
open Ostap.Pretty
open Printf
open Lang
open Helpers

let leftIdented xs = List.map (fun s -> seq [string "  "; s]) xs |! listBy break |! vboxed
let javaHeader globals =
           [  string ".class public HelloWorld";
              string ".super java/lang/Object" ;
              string "";
              globals |! listBy break |! vboxed;
              string "";
              string ".method public <init>()V"  ;
              [ "aload_0";
                "invokenonvirtual java/lang/Object/<init>()V";
                "return" ] |! List.map string |! leftIdented;
              string ".end method";
              string "";
              string ".method public static printInt(I)V";
              leftIdented (List.map string
                             [ ".limit stack 3";
                               "getstatic java/lang/System/out Ljava/io/PrintStream;";
                               "iload_0";
                               "invokevirtual java/io/PrintStream/println(I)V";
                               "return" ]);
              string ".end method";
              string "\n"
           ] |! listBy break |! vboxed

let java_meth_sig args ~name =
  sprintf "HelloWorld/%s(%s)I" name (String.concat "" (List.map (fun _ -> "I") args))

let toJava =
  let binop op l r  = vboxed (listBy break [l; r; seq [string "  "; string op]]) in
  object
    method add         = binop "iadd"
    method mul         = binop "imul"
    method sub         = binop "isub"
    method divide      = binop "idiv"
    method arg_decl    = string

    method assign resolver l r =
      let l =
        match resolver l with
          | Some Global    -> sprintf "  putstatic  HelloWorld/%s I" l
          | Some (Local n) -> sprintf "  istore %d" n
          | _ -> failwith (sprintf "can't resolve variable %s" l)
      in
      [r; string l] |! listBy break |! vboxed

    method literal n     = string (sprintf "  bipush %d" n)

    method ident resolver name    =
      match resolver name with
        | Some Global    -> sprintf "  getstatic HelloWorld/%s I" name |! string
        | Some (Local n) -> sprintf "  iload_%d ; it's an argument `%s`" n name |! string
        | _ -> failwith (sprintf "ident '%s' should be a global or local variable" name)

    method fun_call _ name args =
      [ args |! listBy break |! vboxed;
        seq [string "  invokestatic "; string (java_meth_sig ~name args)]
      ] |! listBy break |! vboxed

    method return_statement x  = x
    method func (name,args,xs) =
      let a = [ string ".method public static "; string name
              ; string "("; String.concat "" (List.map (fun _ -> "I") args) |! string; string ")I"
              ] |! seq |! hboxed in
      let b = vboxed (listBy break xs)  in
      [a;
       string "  .limit stack 100";
       string "  .limit locals 100";
       b;
       string "  ireturn";
       string ".end method"
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
