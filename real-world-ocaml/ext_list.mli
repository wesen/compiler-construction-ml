open Core.Std

include (module type of List)

val intersperse_ : 'a list -> 'a -> 'a list
