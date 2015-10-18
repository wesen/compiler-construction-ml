module type Comparable = sig
  type t
  val compare : t -> t -> int
end

module type Interval = sig
  type t
  type et
  val make : et -> t
end

module Make_interval(Endpoint : Comparable) = struct
  type et = Endpoint.t
  type t = Endpoint.t * Endpoint.t

  let make x = (x, x)
end
