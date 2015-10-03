{
  open Lexing
  open Parser

  exception SyntaxError of string

  let next_line lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <-
      { pos with pos_bol = lexbuf.lex_curr_pos;
                 pos_lnum = pos.pos_lnum + 1
      }
}

let int = '-'? ['0'-'9'] ['0'-'9']*

let digit = ['0'-'9']
let frac = '.' digit*
let exp = ['e' 'E'] ['-' '+']? digit+
let float = digit* frac? exp?
let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

rule read =
  parse
  | white { read lexbuf }
  | newline { next_line lexbuf; read lexbuf }
  | "print" { PRINT }
  | int { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | "type" { TYPE }
  | "var" { VAR }
  | "array of" { ARRAY_OF }
  | "function" { FUNCTION }
  | "print" { PRINT }
  | "let" { LET }
  | "in" { IN }
  | "end" { END }
  | "nil" { NIL }
  | "for" { FOR }
  | "while" { WHILE }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "do" { DO }
  | "to" { TO }
  | "for" { FOR }
  | "/*" { LEFT_COMM }
  | "*/" { RIGHT_COMM }
  | '(' { LEFT_PAREN }
  | ')' { RIGHT_PAREN }
  | '{' { LEFT_BRACE }
  | '}' { RIGHT_BRACE }
  | ":=" { ASSIGN }
  | ',' { COMMA }
  | ':' { COLON }
  | '-' { EQUAL }
  | '.' { DOT }
  | '[' { LEFT_BRACK }
  | ']' { RIGHT_BRACK }
  | ';' { SEMICOLON }
  | '/' { DIV }
  | '+' { PLUS }
  | '-' { MINUS }
  | '*' { STAR }
  | "<>" { DIFF }
  | "<=" { LE }
  | ">=" { GE }
  | '<' { LT }
  | '>' { GT }
  | '&' { BAND }
  | '|' { BOR }
 | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
 | eof { EOF }
