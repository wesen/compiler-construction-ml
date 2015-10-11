open Tiger

open Format

let pp_print_comma ppf () = fprintf ppf ",@ "
let pp_print_s s = fun ppf () -> fprintf ppf "%s@ " s
let pp_print_space ppf () = fprintf ppf "@ "

let op_to_string =
  function
  | PlusOp   -> "+"
  | MinusOp  -> "-"
  | TimesOp  -> "*"
  | DivideOp -> "/"
  | EqOp     -> "="
  | NeqOp    -> "<>"
  | LtOp     -> "<"
  | GtOp     -> ">"
  | LeOp     -> "<="
  | GeOp     -> ">="

let rec pp_var ppf x =
  match x with
  | SimpleVar s -> fprintf ppf "%s" s
  | FieldVar  (v, s) -> fprintf ppf "@[%a@,.%s@]" pp_var v s
  | SubscriptVar (v, e) -> fprintf ppf "@[%a@,[%a]@]" pp_var v pp_exp e
and pp_exp ppf e =
  match e with
  | VarExp (v, pos) -> fprintf ppf "%a" pp_var v
  | NilExp _ -> fprintf ppf "nil"
  | IntExp (i, _) -> fprintf ppf "%d" i
  | StringExp (s, _) -> fprintf ppf "\"%s\"" s
  | CallExp ({func; args}, _)
    -> fprintf ppf "%s(@[<hov>%a@])" func (pp_print_list ~pp_sep:pp_print_comma pp_exp) args
  | OpExp ({left; oper; right}, _)
    -> let pp_paren_exp ppf e = match e with
        | OpExp _ -> fprintf ppf "(@[%a@])" pp_exp e
        | _ -> fprintf ppf "@[%a@]" pp_exp e
    in fprintf ppf "@[%a@ %s@ %a@]" pp_paren_exp left
      (op_to_string oper)
      pp_paren_exp right
  | RecordExp ({fields; typ}, _)
    -> fprintf ppf "@[%s@,{@[<hov>%a@]}@]" (fst typ) pp_fields fields
  | SeqExp exps
    -> fprintf ppf "(@[<hv>%a@])" (pp_print_list ~pp_sep:(pp_print_s ";") pp_exp) exps
  | AssignExp ({var; exp}, _) -> fprintf ppf "@[<hv 2>%a :=@  %a@]" pp_var var pp_exp exp
  | BreakExp _ -> fprintf ppf "break"
  | IfExp ({if_test; then'; else'}, _)
    -> (match else' with
    | None -> fprintf ppf "@[<hv 2>@[if@ %a@]@;<1 -2>then@ %a@]" pp_exp if_test pp_exp then'
    | Some e -> fprintf ppf "@[<hv 2>@[if@ %a@]@;<1 -2>then@ %a@;<1 -2>else@ %a@]"
                  pp_exp if_test pp_exp then' pp_exp e
      )
  | WhileExp ({while_test; while_body}, _)
    -> fprintf ppf "@[<hw 2>while@ %a@;<1 -1>do@ %a@]" pp_exp while_test pp_exp while_body
  | ForExp ({for_var; lo; hi; for_body}, _)
    -> fprintf ppf "@[<hv 2>@[for@ @[@[<hv 2>%s :=@ %a@]@;<1 -2>to@ %a@]@]@;<1 -2>do@ @[%a@]@]"
         for_var pp_exp lo pp_exp hi pp_exp for_body
  | LetExp ({decs; let_body}, _)
    -> fprintf ppf "@[<hv 2>let@ %a@;<1 -2>in@ %a@;<1 -2>end@]"
         pp_decs decs pp_exp let_body
  | _ -> fprintf ppf "..."
and pp_fields ppf fields =
  pp_print_list ~pp_sep:pp_print_comma pp_field ppf fields
and pp_field ppf (s, e, _) = fprintf ppf "@[%s@ =@ %a@]" s pp_exp e
and pp_fun_field ppf {field_name = (n,_); field_typ=(ty,_)} =
  fprintf ppf "@[%s@ %s@]" ty n
and pp_fun_fields ppf fun_fields =
  pp_print_list ~pp_sep:pp_print_comma pp_fun_field ppf fun_fields
and pp_decs ppf decs =
  pp_print_list ~pp_sep:pp_print_space pp_dec ppf decs
and pp_dec ppf d =
  match d with
  | FunctionDec (s, {params; result_typ; fun_body}, _) ->
    fprintf ppf "@[<hv 2>function@ @[%s(@[%a@])@ =@]@ @[%a@]@]"
      s pp_fun_fields params pp_exp fun_body
  | VarDec (s, {var_typ = None; var_init}, _) ->
    fprintf ppf "@[var@ %s@ := @[%a@]@]" s pp_exp var_init
  | VarDec (s, {var_typ = Some (ty, _); var_init}, _) ->
    fprintf ppf "@[var@ @[%s@ :@ %s@]@ := @[%a@]@]" s ty pp_exp var_init
  | TypeDec ((s, _), ty, _) ->
    fprintf ppf "@[type@ @[%s@ =@ %a@]@]" s pp_typ ty
and pp_typ ppf ty =
  match ty with
  | NameTy ((ty, _), _) -> fprintf ppf "%s" ty
  | RecordTy (fs, _) -> fprintf ppf "@[{%a@}@]" pp_fun_fields fs
  | ArrayTy ((ty, _), _)
    -> fprintf ppf "@[array@ of@ %s@]" ty
