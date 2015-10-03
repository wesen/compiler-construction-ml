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

%start <Tiger.stm option> prog
%%


prog:
  | EOF { None }
  | s = stms; EOF { Some s }
  ;

stm:
  | s = SYMBOL; ASSIGN; e = exp { Tiger.AssignStm(s, e) }
  | PRINT; LEFT_PAREN; RIGHT_PAREN { Tiger.PrintStm([]) }
  ;

stms:
  | ss = stms; SEMICOLON; s1 = stm { Tiger.CompoundStm(ss, s1) }
  | s1 = stm { s1 }
    ;

exp :
  | s = SYMBOL { Tiger.IdExp(s) }
  | i = INT { Tiger.NumExp(i) }
  | e1 = exp; b = binop; e2 = exp { Tiger.OpExp(e1, b, e2) }
  | s1 = stm; SEMICOLON; e = exp { Tiger.EseqExp(s1, e) }
    ;

binop :
  | PLUS { Tiger.Plus }
  | MINUS { Tiger.Minus }
  | STAR { Tiger.Times }
  | DIV { Tiger.Div }
  ;

exps: es = separated_list(COMMA, exp) { es } ;
