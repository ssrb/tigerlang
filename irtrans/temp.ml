module type T = sig
type temp  [@@deriving sexp]
type label = Symbol.symbol [@@deriving sexp]

module Table : sig
    type 'a table
    val empty : 'a table
    val look  : 'a table * temp -> 'a option
    val look_exn : 'a table * temp -> 'a
    val enter : 'a table * temp * 'a -> 'a table
end

val newtemp : unit -> temp
val makestring: temp -> string
val newlabel : unit -> label
val namedlabel : string -> label

include Core.Comparable.S with type t := temp

end