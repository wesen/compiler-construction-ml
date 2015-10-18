open Core.Std

module type RESTR =
sig
  val super8 : int -> int
end

module Test:RESTR =
struct
  module B =
  struct
    let b x = x * 10
  end

  let super8 x = B.b x
end

module type CONTROL_ACCT_NUM =
sig
  val set_base : string -> unit
  val next     : unit   -> string
  val reset    : unit   -> unit
end

module type ACCT_NUM =
sig
  val next: unit -> string
end

module Cacc_num:CONTROL_ACCT_NUM =
struct
  type counter = {
    mutable base  : string;
    mutable count : int
  }

  let base = {
    base = "base";
    count = 0
  }

  let set_base x = base.base <- x
  let base_to_string x = Printf.sprintf "%s-%i" x.base x.count
  let next () = base.count <- base.count + 1; base_to_string base
  let reset () = base.count <- 0
end

module Uacc_num = (Cacc_num:ACCT_NUM)
