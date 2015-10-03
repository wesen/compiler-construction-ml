%{
open Codegen
open Core.Std
%}

%token BEGIN END EOF
%token <string> IDENTIFIER
%token <int> LITERAL
%token READ WRITE
%token ASSIGN
%token LEFT_PAREN RIGHT_PAREN
%token ADD_OP SUB_OP
%token COMMA SEMICOLON

%start program
%type <unit> program

%%

program:
    | begin_stmt statements end_stmt EOF { raise End_of_file }
    ;

begin_stmt:
  | BEGIN { generate_begin () }
  ;

end_stmt:
  | END { generate_end () }
  ;

statements:
  | { }
  | statement SEMICOLON statements { }
  ;

statement:
  | IDENTIFIER ASSIGN expression { }
  | READ LEFT_PAREN identifier_list RIGHT_PAREN { printf "read " }
  | WRITE LEFT_PAREN expression_list RIGHT_PAREN { printf "write " }
  ;

identifier_list:
  | IDENTIFIER { printf "identifier: %s\n" $1; [$1] }
  | IDENTIFIER COMMA identifier_list { $1 :: $3 }
  ;

expression_list:
  | expression { [$1] }
  | expression COMMA expression_list { $1 :: $3 }
  ;

expression:
  | IDENTIFIER { $1 }
  | LITERAL { string_of_int $1 }
  | addop { $1 }
  | subop { $1 }
  ;

addop:
  | LITERAL ADD_OP LITERAL { string_of_int ($1 + $3) }
  | expression ADD_OP expression { "addop" }
  ;

subop:
  | LITERAL SUB_OP LITERAL { string_of_int ($1 - $3) }
  | expression SUB_OP expression { "subop" }
  ;
