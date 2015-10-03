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
  | s = STRING { StringExp (s, makepos $startpos $endpos) }
  | i = INT { IntExp (i, makepos $startpos $endpos) }
  | NIL { NilExp (makepos $startpos $endpos) }
  | v = lvalue { VarExp (v, makepos $startpos $endpos) }
  | MINUS; e = expr {
                   OpExp ({ left  = IntExp (0, makepos $startpos $startpos);
                            oper  = MinusOp;
                            right = e; },
                          makepos $startpos $endpos) }
  | e1 = expr; o = op; e2 = expr { OpExp ({ left  = e1;
                                            oper  = o;
                                            right = e2 },
                                          makepos $startpos $endpos) }
  ;

%inline op:
  | DIFF { NeqOp }
  | EQUAL { EqOp }
  | LE { LeOp }
  | GE { GeOp }
  | LT { LtOp }
  | GT { GtOp }
  | PLUS { PlusOp }
  | MINUS { MinusOp }
  | STAR { TimesOp }
  | DIV { DivideOp }
  ;

lvalue:
  | s = SYMBOL { SimpleVar s }
  | l = lvalue; DOT; s = SYMBOL { FieldVar (l, s) }
  | l = lvalue; LEFT_BRACK; e = expr; RIGHT_BRACK { SubscriptVar (l, e) }
  ;

exprs:
  | e1 = expr; SEMICOLON; e2 = exprs { match e2 with
                                       | SeqExp es -> SeqExp (e1::es)
                                       | _ -> SeqExp [e1; e2]
                                    }
  | e = expr { e }
    ;
