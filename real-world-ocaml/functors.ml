open Core.Std

module type Comparable = sig
  type t
  val compare : t -> t -> int
end

module type Interval_intf = sig
  type t
  type endpoint
  val create    : endpoint -> endpoint -> t
  val is_empty  : t -> bool
  val contains  : t -> endpoint -> bool
  val intersect : t -> t -> t
end

module Make_interval(Endpoint : Comparable) :
  (** sharing constraint *)
  (Interval_intf with type endpoint = Endpoint.t) = struct
  type endpoint = Endpoint.t
  type t = | Interval of Endpoint.t * Endpoint.t
           | Empty

  (** [create low high] creates a new interval from [low] to [high].
      If [low > high], then the interval is empty *)
  let create low high =
    if Endpoint.compare low high > 0 then Empty
    else Interval (low, high)

  let is_empty = function
    | Empty -> true
    | Interval _ -> false

  let contains t x =
    match t with
    | Empty -> false
    | Interval (l, h) ->
      Endpoint.compare x l >= 0 && Endpoint.compare x h <= 0

  let intersect t1 t2 =
    let min x y = if Endpoint.compare x y <= 0 then x else y in
    let max x y = if Endpoint.compare x y >= 0 then x else y in
    match t1, t2 with
    | Empty , _ | _, Empty -> Empty
    | Interval (l1, h1), Interval (l2, h2) ->
      create (max l1 l2) (min h1 h2)
end

module Int_interval =
  Make_interval(struct
    type t = int
    let compare = Int.compare
  end)

module Int_interval = Make_interval(Int)
module String_Interval = Make_interval(String)

(** destructive substitution *)
module type Int_interval_intf =
  Interval_intf with type endpoint := int

module Make_interval(Endpoint : Comparable) :
  (** destructive substitution *)
  (Interval_intf with type endpoint := Endpoint.t) = struct
  type t = | Interval of Endpoint.t * Endpoint.t
           | Empty

  (** [create low high] creates a new interval from [low] to [high].
      If [low > high], then the interval is empty *)
  let create low high =
    if Endpoint.compare low high > 0 then Empty
    else Interval (low, high)

  let is_empty = function
    | Empty -> true
    | Interval _ -> false

  let contains t x =
    match t with
    | Empty -> false
    | Interval (l, h) ->
      Endpoint.compare x l >= 0 && Endpoint.compare x h <= 0

  let intersect t1 t2 =
    let min x y = if Endpoint.compare x y <= 0 then x else y in
    let max x y = if Endpoint.compare x y >= 0 then x else y in
    match t1, t2 with
    | Empty , _ | _, Empty -> Empty
    | Interval (l1, h1), Interval (l2, h2) ->
      create (max l1 l2) (min h1 h2)
end

type some_type = int * string list with sexp

let a = sexp_of_some_type (33, ["one"; "two"])
let b = Sexp.of_string "(44 (five six))" |> some_type_of_sexp

module type Interval_intf_with_sexp = sig
  type t
  include Interval_intf with type t := t
  include Sexpable      with type t := t
end

module Make_interval(Endpoint : sig
    type t
    include Comparable with type t := t
    include Sexpable with type t := t
  end)
  : (Interval_intf_with_sexp with type endpoint := Endpoint.t) = struct
  type t = | Interval of Endpoint.t * Endpoint.t
           | Empty
  with sexp

  (** [create low high] creates a new interval from [low] to [high].
      If [low > high], then the interval is empty *)
  let create low high =
    if Endpoint.compare low high > 0 then Empty
    else Interval (low, high)

  let is_empty = function
    | Empty -> true
    | Interval _ -> false

  let contains t x =
    match t with
    | Empty -> false
    | Interval (l, h) ->
      Endpoint.compare x l >= 0 && Endpoint.compare x h <= 0

  let intersect t1 t2 =
    let min x y = if Endpoint.compare x y <= 0 then x else y in
    let max x y = if Endpoint.compare x y >= 0 then x else y in
    match t1, t2 with
    | Empty , _ | _, Empty -> Empty
    | Interval (l1, h1), Interval (l2, h2) ->
      create (max l1 l2) (min h1 h2)

  (** override the sexp converter to make sure the interval invariant is
      preserved *)
  let t_of_sexp sexp =
    match t_of_sexp sexp with
    | Empty -> Empty
    | Interval (x, y) -> create x y
end

module type EXAMPLE =
sig
  type maybe
  val plus : int * int -> int * int -> int * int
  val minus : int * int -> int * int -> int * int
end


module Dbg_E:EXAMPLE =
struct
  type maybe = X | Y

  let plus (x1,x2) (y1,y2) =
    Printf.printf "%i %i\n%i %i\n" x1 x2 y1 y2;
    (x1, y1)

  let minus (x1,x2) (y1,y2) =
    Printf.printf "%i %i\n%i %i\n" x1 x2 y1 y2;
    (x2, y2)
end

module type F_TOR = functor (E: EXAMPLE) ->
sig
  val plus : int -> int -> int -> int -> int * int
  val minus : int -> int -> int -> int -> int * int
end

module F_tor: F_TOR = functor (E:EXAMPLE) ->
struct
  let plus x y z m = E.plus (x,y) (z,m)
  let minus x y z m = E.minus (x,y) (z,m)
end

module F = F_tor(struct
    type maybe = A | B
    let plus x y = (fst x, fst y)
    let minus x y = (snd y, snd x)
  end)

module F' = F_tor(Dbg_E)
