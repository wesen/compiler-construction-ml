exception ParseError of string

type pos = int * int
type symbol = string

type var = SimpleVar    of symbol
         | FieldVar     of var * symbol
         | SubscriptVar of var * exp
and call_exp = { func: symbol;
                 args: exp list }
and op_exp = { left  : exp;
               oper  : oper;
               right : exp }
and record_exp = { fields: (symbol * exp * pos) list;
                   typ: type_id }
and assign_exp = { var: var;
                   exp: exp }
and if_exp = { if_test: exp;
               then': exp;
               else': exp option }
and while_exp = { while_test: exp;
                  while_body: exp }
and for_exp = { for_var: symbol;
                (* for_escape: bool ref; *)
                lo: exp;
                hi: exp;
                for_body: exp }
and let_exp = { decs: dec list;
                let_body: exp }
and array_exp = { array_typ : type_id;
                  size      : exp;
                  init      : exp }
and exp =
    VarExp    of var        * pos
  | NilExp    of pos
  | IntExp    of int        * pos
  | StringExp of string     * pos
  | CallExp   of call_exp   * pos
  | OpExp     of op_exp     * pos
  | RecordExp of record_exp * pos
  | SeqExp    of exp list
  | AssignExp of assign_exp * pos
  | IfExp     of if_exp     * pos
  | WhileExp  of while_exp  * pos
  | ForExp    of for_exp    * pos
  | BreakExp  of pos
  | LetExp    of let_exp    * pos
  | ArrayExp  of array_exp  * pos
and type_id = symbol * pos
and var_dec = { (* var_escape: bool ref; *)
                var_typ: type_id option;
                var_init: exp }
and fun_dec = { params: field list;
                result_typ: type_id option;
                fun_body: exp }
and dec =
    FunctionDec of (symbol * fun_dec * pos)
  | VarDec of symbol * var_dec * pos
  | TypeDec of ((symbol * pos) * ty * pos)
and oper = PlusOp | MinusOp | TimesOp | DivideOp
         | EqOp | NeqOp | LtOp | LeOp | GtOp | GeOp
and field = {
  field_name: symbol * pos;
  (* field_escape: bool ref; *)
  field_typ: symbol * pos }
and ty = NameTy of type_id * pos
       | RecordTy of field list * pos
       | ArrayTy of type_id * pos

let rec exp_to_string =
  let open Printf in
  function
  | VarExp (v, _) -> var_to_string v
  | NilExp _ -> "nil"
  | IntExp (i, _) -> sprintf "%d" i
  | StringExp (s, _) -> sprintf "\"%s\"" s
  | CallExp ({func; args;}, _) ->
    sprintf "(%s %s)" func (String.concat " " (List.map exp_to_string args))
  | OpExp ({left; oper; right}, _) ->
    sprintf "(%s) %s (%s)" (exp_to_string left) (op_to_string oper) (exp_to_string right)

  | _ -> ""

and var_to_string =
  let open Printf in
  function
  | SimpleVar s -> s
  | FieldVar (v, s) -> sprintf "%s.%s" (var_to_string v) s
  | SubscriptVar (v, e) ->
    sprintf "%s[%s]" (var_to_string v) (exp_to_string e)

and op_to_string =
  function
  | PlusOp -> "+"
  | MinusOp -> "-"
  | TimesOp -> "*"
  | DivideOp -> "/"
  | EqOp -> "="
  | NeqOp -> "<>"
  | LtOp -> "<"
  | GtOp -> ">"
  | LeOp -> "<="
  | GeOp -> ">="
