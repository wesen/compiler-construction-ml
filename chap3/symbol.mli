type t

val symbol : string -> t
val name : t -> string

type 'a table
val empty : 'a table
val enter : 'a table -> t -> 'a -> 'a table
val look : 'a table -> t -> 'a option
