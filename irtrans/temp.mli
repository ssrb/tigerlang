type temp
type label = Symbol.symbol
val newtemp : unit -> temp
(* structure Table : TABLE sharing type Table.key = temp *)
val makestring: temp -> string
val newlabel : unit -> label
val namedlabel : string -> label

