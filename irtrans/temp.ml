module type T = sig
type temp
type label = Symbol.symbol
val newtemp : unit -> temp
val makestring: temp -> string
val newlabel : unit -> label
val namedlabel : string -> label
end