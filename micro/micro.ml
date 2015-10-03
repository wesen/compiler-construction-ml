open Parser
open Lexer
open Lexing
open Core.Std

let compile f =
  let lexbuf = Lexing.from_channel (open_in f) in
  try
    let rec parse () =
      Parser.program Lexer.micro lexbuf; parse () in
    ignore(parse())
  with
    End_of_file ->
    begin
      printf "End of file\n"
    end
  | Lexer.Syntax_error s ->
     printf "%s" s;
     exit 1

let help () = print_string "micro <file>\n"

let () = if Array.length Sys.argv = 1 then help ()
         else
           let file = Array.get Sys.argv 1 in
           printf "compiling %s\n" file;
           compile file
