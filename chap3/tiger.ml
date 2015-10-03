type pos = int * int
type symbol = string

type var = SimpleVar of symbol
         | FieldVar of var * symbol
         | SubscriptVar of var * exp
and call_exp = { func: symbol;
                 args: exp list }
and op_exp = { left: exp;
               oper: oper;
               right: exp;
               pos: pos }
and record_exp = { fields: (symbol * exp * pos) list;
                   typ: symbol }
and assign_exp = { var: var;
                   exp: exp }
and if_exp = { if_test: exp;
               then': exp;
               else': exp option }
and while_exp = { while_test: exp;
                  while_body: exp }
and for_exp = { for_var: symbol;
                for_escape: bool ref;
                lo: exp;
                hi: exp;
                for_body: exp }
and let_exp = { decs: dec list;
                let_body: exp }
and array_exp = { array_typ: symbol;
                  size: exp;
                  init: exp }
and exp =
    VarExp of var * pos
  | NilExp of pos
  | IntExp of int * pos
  | StringExp of string * pos
  | CallExp of call_exp * pos
  | OpExp of op_exp * pos
  | RecordExp of record_exp * pos
  | SeqExp of exp list
  | AssignExp of assign_exp * pos
  | IfExp of if_exp * pos
  | WhileExp of while_exp * pos
  | ForExp of for_exp * pos
  | BreakExp of pos
  | LetExp of let_exp * pos
  | ArrayExp of array_exp * pos
and var_dec = { var_escape: bool ref;
                var_typ: (symbol * pos) option;
                var_init: exp }
and fun_dec = { params: field list;
                result: (symbol * pos) option;
                fun_body: exp }
and dec =
    FunctionDec of (symbol * fun_dec * pos) list
  | VarDec of symbol * var_dec * pos
  | TypeDec of (symbol * ty * pos) list
and oper = PlusOp | MinusOp | TimesOp | DivideOp
         | EqOp | NeqOp | LtOp | LeOp | GtOp | GeOp
and field = { field_escape: bool ref;
              field_typ: symbol }
and ty = NameTy of symbol * pos
       | RecordTy of (symbol * field * pos) list
       | ArrayTy of symbol * pos
