{
  open Parser

  let keywords = [
      "begin", BEGIN;
      "end", END;
      "read", READ;
      "write", WRITE
    ]

  (* current token line number *)
  let line_num = ref 1
  exception Syntax_error of string

  let syntax_error msg = raise (Syntax_error (msg ^ " on line " ^ (string_of_int !line_num)))

}

let blank = [' ' '\r' '\t']
let digit = ['0'-'9']
let digits = digit*
               let alpha = ['a'-'z' 'A'-'Z']
let iden = alpha (alpha | digit | '_')*

rule micro = parse
         | ":=" { ASSIGN }
         | '+' { ADD_OP }
         | '-' { SUB_OP }
         | ',' { COMMA }
         | ';' { SEMICOLON }
         | '(' { LEFT_PAREN }
         | ')' { RIGHT_PAREN }
         | digits as d {
                         (* parse literal *)
                         LITERAL (int_of_string d)
                       }
         | '\n' { incr line_num; micro lexbuf } (* counting new line characters *)
         | blank { micro lexbuf } (* skipping blank characters *)
         | _ { syntax_error "couldn't identify the token" }
         | eof { EOF }
         | iden as i {
                       (* try keywords if not found then it's identifier *)
                       let l = String.lowercase i in
                       try List.assoc l keywords
                       with Not_found -> IDENTIFIER l
                     }
