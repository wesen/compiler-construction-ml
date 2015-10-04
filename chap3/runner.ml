open Core.Std
open Lexer
open Lexing

let loop filename () =
  output_string stdout (Tiger.exp_to_string (Chap3.parse_file filename));
  output_string stdout "\n"

let () =
  Command.basic ~summary:"Parse and display Tiger"
    Command.Spec.(empty +> anon ("filename" %: file))
    loop
  |> Command.run  (* ~version:"1.0" ~build_info:"FOO" *)
