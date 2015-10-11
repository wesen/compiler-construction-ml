open Core.Std

type symbol = string * int

let foo = 1
let nextsym = ref 0
let hashtable : (string, int) Hashtbl.create ~hashable:String.hashable ()
