module type T = sig

module Temp : Temp.T
module Tree : Tree.T with module Temp = Temp

type frame [@@deriving sexp]
type access  [@@deriving sexp]
type frag = PROC of {body: Tree.stm; frame: frame} | STRING of Temp.label * string

val fp: Temp.temp
val rv: Temp.temp
val wordSize: int

val newFrame: name:Temp.label -> formals:bool list -> frame
val name: frame -> Temp.label
val formals: frame -> access list
val allocLocal: frame -> bool -> access
val exp: access * Tree.exp -> Tree.exp
val procEntryExit1: frame * Tree.stm -> Tree.stm

end
