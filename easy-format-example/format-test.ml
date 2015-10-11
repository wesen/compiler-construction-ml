type lambda =
  | Lambda of string * lambda
  | Var of string
  | Apply of lambda * lambda

open Format

let ident = print_string
let kwd = print_string

(*
val ident : string -> unit = <fun>
val kwd : string -> unit <fun>
   *)

let rec print_exp0 = function
  | Var s -> ident s
  | lam -> open_hovbox 1; kwd "("; print_lambda lam; kwd ")"; close_box ()

and print_app = function
  | e -> open_hovbox 2; print_other_applications e; close_box ()

and print_other_applications f =
  match f with
  | Apply (f, arg) -> print_app f; print_space (); print_exp0 arg
  | f -> print_exp0 f

and print_lambda = function
  | Lambda (s, lam) ->
    open_hovbox 1;
    kwd "\\"; ident s; kwd "."; print_space (); print_lambda lam;
    close_box()
  | e -> print_app e
