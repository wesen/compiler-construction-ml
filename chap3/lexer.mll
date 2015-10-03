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

let decimal_char s = char_of_int (int_of_string (String.sub s 1 ((String.length s) - 1)))
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
       | int { INT (int_of_string (Lexing.lexeme lexbuf)) }
       | '"' { read_string (Buffer.create 17) lexbuf }

       | "type" { TYPE }
       | "var" { VAR }
       | "array of" { ARRAY_OF }
       | "function" { FUNCTION }
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

       | "/*" { ignore(read_comment 0 lexbuf); read lexbuf }

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

       | id { SYMBOL (Lexing.lexeme lexbuf) }

       | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
       | eof { EOF }

and read_string buf =
  parse
| '"' { STRING (Buffer.contents buf) }
| '\\' 'n' { Buffer.add_char buf '\n'; read_string buf lexbuf }
| '\\' 't' { Buffer.add_char buf '\t'; read_string buf lexbuf }
| '\\' '"' { Buffer.add_char buf '"'; read_string buf lexbuf }
| '\\' digit+ { Buffer.add_char buf (decimal_char (Lexing.lexeme lexbuf));
               read_string buf lexbuf }
| '\\' (white|newline)* '\\' { read_string buf lexbuf }
| [^ '"' '\\']+ { Buffer.add_string buf (Lexing.lexeme lexbuf); read_string buf lexbuf }
| _ { raise (SyntaxError ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
| eof { raise (SyntaxError ("String is not terminated")) }

and read_comment level =
  parse
| "*/" { if level == 0 then () else (read_comment (level - 1) lexbuf) }
| "/*" { read_comment (level + 1) lexbuf }
| eof { raise (SyntaxError ("Comment is not terminated")) }
| _ { read_comment level lexbuf }

(*
#directory "/Users/manuel/code/wesen/compiler-construction-ml/chap3/_build";;
#load_rec "chap3.cmo";;
open Chap3;;
   *)
