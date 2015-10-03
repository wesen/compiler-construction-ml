type value = [
  | `Int of int
  | `String of string
  | `Symbol of string
  ]

type id = string

type binop = Plus
           | Minus
           | Times
           | Div

type stm = CompoundStm of stm * stm
         | AssignStm of id * exp
         | PrintStm of exp list

 and exp = IdExp of id
         | NumExp of int
         | OpExp of exp * binop * exp
         | EseqExp of stm * exp

open Core.Std

let rec output_stm outc = function
    | CompoundStm (s1, s2) -> printf "compound"
    | AssignStm (id, e1) -> printf "assign(%s, %a)" id output_exp e1
    | PrintStm l -> printf "print"

  and output_exp outc e =
    printf "exp"
