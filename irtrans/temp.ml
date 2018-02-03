module type T = sig
type temp  [@@deriving sexp]
type label = Symbol.symbol [@@deriving sexp]
val newtemp : unit -> temp
val makestring: temp -> string
val newlabel : unit -> label
val namedlabel : string -> label
end