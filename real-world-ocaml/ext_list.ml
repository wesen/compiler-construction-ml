open Core.Std

let rec intersperse_ list el =
  match list with
  | [] | [ _ ] -> list
  | x :: y :: tl -> x :: el :: intersperse_ (y::tl) el

include List
