type lambda =
  | Lambda of string * lambda
  | Var of string
  | Apply of lambda * lambda

open Format

let ident ppf s = fprintf ppf "%s" s
let kwd ppf s = fprintf ppf "%s" s

let rec pr_exp0 ppf = function
  | Var s -> fprintf ppf "%a" ident s
  | lam -> fprintf ppf "@[<1>(%a)@]" pr_lambda lam
and pr_app ppf = function
  | e -> fprintf ppf "@[<2>%a@]" pr_other_applications e
and pr_other_applications ppf f =
  match f with
  | Apply (f, arg) -> fprintf ppf "%a@ %a" pr_app f pr_exp0 arg
  | f -> pr_exp0 ppf f
and pr_lambda ppf = function
  | Lambda (s, lam) ->
    fprintf ppf "@[<1>%a%a%a@ %a@]" kwd "\\" ident s kwd "." pr_lambda lam
  | e -> pr_app ppf e
