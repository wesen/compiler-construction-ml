%{
    open Tiger

    let makepos start' end' = (start'.Lexing.pos_cnum, end'.Lexing.pos_cnum)
%}

%token <int> INT
%token <string> SYMBOL

%token <float> FLOAT
%token <string> STRING

%token TYPE
%token ARRAY_OF
%token VAR
%token FUNCTION
%token NIL
%token IF
%token THEN
%token ELSE
%token WHILE
%token DO
%token FOR
%token TO
%token BREAK
%token LET
%token IN
%token END

%token LEFT_PAREN
%token RIGHT_PAREN
%token LEFT_BRACE
%token RIGHT_BRACE
%token LEFT_BRACK
%token RIGHT_BRACK

%token LEFT_COMM
%token RIGHT_COMM

%token ASSIGN
%token COMMA
%token SEMICOLON

%token COLON
%token EQUAL
%token DOT

%token PLUS
%token MINUS
%token STAR
%token DIV

%token LT
%token LE
%token GT
%token GE
%token DIFF

%token BAND
%token BOR

%token EOF

%token PRINT

%start <Tiger.exp option> prog
%%


prog:
  | EOF { None }
  | e = exprs; EOF { Some e }
  ;

expr:
  | s = SYMBOL { VarExp (SimpleVar s, makepos $startpos $endpos) }
  | i = INT { IntExp (i, makepos $startpos $endpos) }
  ;

exprs:
  | e1 = expr; SEMICOLON; e2 = exprs { match e2 with
                                       | SeqExp es -> SeqExp (e1::es)
                                       | _ -> SeqExp [e1; e2]
                                    }
  | e = expr { e }
    ;
