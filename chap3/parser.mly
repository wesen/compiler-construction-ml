%{
    open Tiger

    let makepos start' end' = (start'.Lexing.pos_cnum, end'.Lexing.pos_cnum)
%}

%token <int> INT
%token <string> SYMBOL

%token <string> STRING

%token TYPE
%token ARRAY
%token OF
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

%nonassoc ASSIGN
%nonassoc THEN
%nonassoc ELSE
%nonassoc DO
%nonassoc OF

%nonassoc DIFF EQUAL
%nonassoc LE GE LT GT
%left BOR
%left BAND
%left PLUS MINUS
%left STAR DIV
%left UMINUS

%start <Tiger.exp option> prog
%%


prog:
  | EOF { None }
  | e = expr; EOF { Some e }
  ;

expr:
  | s = STRING { StringExp (s, makepos $startpos $endpos) }
  | i = INT { IntExp (i, makepos $startpos $endpos) }
  | NIL { NilExp (makepos $startpos $endpos) }
  | v = lvalue { VarExp (v, makepos $startpos $endpos) }
  | l = symbol_with_pos { match l with
                          | (l, s) -> VarExp (SimpleVar l, s) }

  (* shift reduce conflict, averted with %left ASSIGN *)
  | v = lvalue; ASSIGN; e = expr { AssignExp ({var = v; exp = e;}, makepos $startpos $endpos) }

  | MINUS; e = expr %prec UMINUS {
                   OpExp ({ left  = IntExp (0, makepos $startpos $startpos);
                            oper  = MinusOp;
                            right = e; },
                          makepos $startpos $endpos) }
  | e1 = expr; BAND; e2 = expr { IfExp ({ if_test = e1;
                                          then' = e2;
                                          else' = Some (IntExp (0, makepos $startpos $endpos)) },
                                        makepos $startpos $endpos) }
  | e1 = expr; BOR; e2 = expr { IfExp ({ if_test = e1;
                                         then' = IntExp (1, makepos $startpos $endpos);
                                         else' = Some e2 },
                                       makepos $startpos $endpos) }
  | e1 = expr; o = op; e2 = expr { OpExp ({ left  = e1;
                                            oper  = o;
                                            right = e2 },
                                          makepos $startpos $endpos) }
  | s = SYMBOL; LEFT_PAREN; args = separated_list(COMMA, expr); RIGHT_PAREN
         { CallExp ({func = s;
                     args = args;}, makepos $startpos $endpos) }

  | LEFT_PAREN; es = exprs; RIGHT_PAREN { es }

  | ty = symbol_with_pos; LEFT_BRACE;
    fields = fields; RIGHT_BRACE {
                             RecordExp ({fields = fields; typ = ty; }, makepos $startpos $endpos)
                       }

  | be = brack_exp;
    OF; e2 = expr {
                 match be with
                 | ((s, pos), e) ->
                    ArrayExp ({array_typ = (s, pos);
                               size = e;
                               init = e2},
                              makepos $startpos $endpos)
               }

  (* shift reduce conflict, one averted by %left ELSE *)
  | IF; test = expr; THEN; body = expr { IfExp ({if_test = test; then' = body; else' = None;}, makepos $startpos $endpos) }
  | IF; test = expr; THEN; e1 = expr; ELSE; e2 = expr { IfExp ({if_test = test; then' = e1; else' = Some e2;}, makepos $startpos $endpos) }

  (* shift reduce conflict, averted by %left DO *)
  | WHILE; test = expr; DO; body = expr { WhileExp ({ while_test = test; while_body = body; }, makepos $startpos $endpos) }
  | BREAK { BreakExp (makepos $startpos $endpos ) }

  | FOR; s = SYMBOL; ASSIGN; lo = expr; TO; hi = expr; DO body = expr {
                                                                     ForExp ({ for_var = s;
                                                                               lo = lo;
                                                                               hi = hi;
                                                                               for_body = body; }, makepos $startpos $endpos) }

  | LET; decls = nonempty_list(decl); IN; es = exprs; END { LetExp ({decs = decls; let_body = es;}, makepos $startpos $endpos) }
    ;

brack_exp:
  | ty = symbol_with_pos; LEFT_BRACK; e1 = expr; RIGHT_BRACK { (ty, e1) }
    ;

fields:
  | fs = separated_list(COMMA, field) { fs }
  ;

field:
  | s = SYMBOL; EQUAL; e = expr { (s, e, makepos $startpos $endpos) }
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

decl:
  | VAR; s = SYMBOL; ASSIGN; e = expr { VarDec (s, { var_typ = None; var_init = e }, makepos $startpos $endpos ) }

  | VAR; s = SYMBOL; COLON; ty = symbol_with_pos;
    ASSIGN; e = expr {
                    VarDec (s, { var_typ = Some ty;
                                 var_init = e; },
                            makepos $startpos $endpos ) }

  | TYPE; id = symbol_with_pos; EQUAL; ty = typedef { TypeDec (id, ty, makepos $startpos $endpos) }

  | FUNCTION; s = SYMBOL; LEFT_PAREN; fs = type_fields; RIGHT_PAREN;
    EQUAL; e = expr{
                   FunctionDec (s, { params = fs;
                                     result_typ = None;
                                     fun_body = e; },
                                makepos $startpos $endpos) }
  | FUNCTION; s = SYMBOL; LEFT_PAREN; fs = type_fields; RIGHT_PAREN;
    COLON; ty = symbol_with_pos;
    EQUAL; e = expr {
                   FunctionDec (s, { params = fs;
                                     result_typ = Some ty;
                                     fun_body = e; },
                                makepos $startpos $endpos) }
    ;

typedef:
  | ty = symbol_with_pos { NameTy (ty, makepos $startpos $endpos) }
  | LEFT_BRACE; ts = type_fields; RIGHT_BRACE { RecordTy (ts, makepos $startpos $endpos) }
  | ARRAY; OF; ty = symbol_with_pos { ArrayTy (ty, makepos $startpos $endpos)  }
    ;

type_fields:
  | fs = separated_list(COMMA, type_field) { fs }
  ;

field_name:
  | s = SYMBOL { (s, makepos $startpos $endpos) }
  ;

type_field:
  | s = field_name; COLON; ty_id = symbol_with_pos { {field_name = s; field_typ = ty_id;} }
  ;

symbol_with_pos:
  | s = SYMBOL { (s, makepos $startpos $endpos) }
  ;

lvalue:
  | l = lvalue; DOT; s = SYMBOL { FieldVar (l, s) }
  | l = symbol_with_pos; DOT; s = SYMBOL { match l with
                                         | (l, _) -> FieldVar (SimpleVar l, s) }
  | be = brack_exp { match be with
                     | ((s, _), e) -> SubscriptVar (SimpleVar s, e)
                   }
  | l = lvalue; LEFT_BRACK; e = expr; RIGHT_BRACK { SubscriptVar (l, e) }
  ;

    (* verbose way of doing it *)
exprs:
  | e1 = expr; SEMICOLON; e2 = exprs { match e2 with
                                       | SeqExp es -> SeqExp (e1::es)
                                       | _ -> SeqExp [e1; e2]
                                    }
  | e = expr { e }
    ;
