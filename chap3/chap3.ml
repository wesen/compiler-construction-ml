open Core.Std
open Lexer
open Lexing

let print_position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  fprintf outx "%s:%d:%d" pos.pos_fname
          pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let parse_with_error lexbuf =
  try Parser.prog Lexer.read lexbuf with
  | SyntaxError msg ->
     fprintf stderr "%a: %s\n" print_position lexbuf msg;
     None
  | Parser.Error ->
    fprintf stderr "%a: syntax error\n" print_position lexbuf;
    None

let parse_and_print lexbuf =
  match parse_with_error lexbuf with
  | Some exp ->
     Tiger.exp_to_string exp
  | None -> raise (Lexer.SyntaxError "syntax error")

(*
let loop filename () =
  let inx = In_channel.create filename in
  let lexbuf = Lexing.from_channel inx in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  parse_and_print lexbuf;
  In_channel.close inx
   *)

let lexbuf_from_file filename =
  let inx = In_channel.create filename in
  Lexing.from_channel inx

let parse_file filename =
  let lexbuf = lexbuf_from_file filename in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  match parse_with_error lexbuf with
  | Some v -> v
  | None -> raise (Lexer.SyntaxError "syntax error")

let parse_string s =
  let lexbuf = Lexing.from_string s in
  match parse_with_error lexbuf with
  | Some v -> v
  | None -> raise (Lexer.SyntaxError "syntax error")

let lex_one s =
  let lexbuf = Lexing.from_string s in
  (Lexer.read lexbuf, Lexer.read lexbuf)

let lex_all lexbuf =
  let rec lex' lexbuf =
    match Lexer.read lexbuf with
    | Parser.EOF -> []
    | _ as s -> s :: (lex' lexbuf)
  in lex' lexbuf

let lex_string s =
  let lexbuf = Lexing.from_string s in
  lex_all lexbuf

let lex_file filename =
  let lexbuf = lexbuf_from_file filename in
  lex_all lexbuf

(*
let () =
  Command.basic ~summary:"Parse and display Tiger"
    Command.Spec.(empty +> anon ("filename" %: file))
    loop
  |> Command.run  (* ~version:"1.0" ~build_info:"FOO" *)
*)
