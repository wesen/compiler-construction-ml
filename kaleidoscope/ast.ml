type expr =
  | Number of float
  | Variable of string
  | Binary of char * expr * expr
  | Call of string * expr array

(* function prototype, name and argument names *)
type proto = Prototype of string * string array

(* function definition itself *)
type func = Function of proto * expr
