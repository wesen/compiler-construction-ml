(* Lexer tokens *)
type token =
   (* commands *)
   | Def | Extern
   (* Primary *)
   | Ident of string | Number of float
   (* Unknown *)
   | Kwd of char
