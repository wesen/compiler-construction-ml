module type SymbolSig = sig
  type t

  val symbol : string -> t
  val name : t -> string
end

module type MutableSymbolTable = sig
  type t

  type 'a table
  val create : unit -> 'a table
  val enter : 'a table -> t -> 'a -> unit
  val remove : 'a table -> 'a -> unit
  val look : 'a table -> 'a -> 'a option
end

module Symbol : SymbolSig = struct
  open Core.Std

  type t = int with compare, sexp

  let nextsym = ref 0

  let symtbl = String.Table.create ()
  let inv_symtbl = Int.Table.create ()
  let symbol s = match Hashtbl.find symtbl s with
    | Some x -> x
    | None ->
      let sym = !nextsym
      in (nextsym := sym + 1;
          Hashtbl.set symtbl ~key:s ~data:sym;
          Hashtbl.set inv_symtbl ~key:sym ~data:s;
          sym)

  let name sym = match Hashtbl.find inv_symtbl sym with
    | Some x -> x
    | None -> ""
end

module StringIntSymbolTable : MutableSymbolTable = struct
  open Core.Std

  type t = int
  type 'a table = (t, 'a) Hashtbl.Poly.t

  let create = Hashtbl.Poly.create
  let enter tbl s data = Hashtbl.set tbl ~key:s ~data
  let remove (tbl : v table) (s : t) = Hashtbl.remove tbl s
  let look tbl s = Hashtbl.find tbl s
end

module type TypeWrapper = sig
  type t
end
