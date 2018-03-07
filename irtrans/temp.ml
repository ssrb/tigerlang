module type T = sig
type temp  [@@deriving sexp]
type label = Symbol.symbol [@@deriving sexp]
module Table : sig
    type 'a table
    val empty : 'a table
    val look  : 'a table * temp -> 'a option
    val enter : 'a table * temp * 'a -> 'a table
end
module Comp : Core.Comparator.S with type t := temp
val newtemp : unit -> temp
val makestring: temp -> string
val newlabel : unit -> label
val namedlabel : string -> label
end