open Core.Std

module type SymbolMapSig = sig
  type t

  type 'a table

  val empty : 'a table
end

module SymbolMap : SymbolMapSig = struct
  type t = int

  type 'a table = 'a Int.Map.t

  let empty = Int.Map.empty
end

module type SymbolHashSig = sig
  type t

  type 'a table

  val create : unit -> 'a table
end

module SymbolHash : SymbolHashSig = struct
  type t = int

  type 'a table = (int, 'a) Hashtbl.t

  let create () = Int.Table.create ()
end
