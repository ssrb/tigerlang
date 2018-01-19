module type T = sig

module Tree : Tree.T
module Temp = Tree.Temp

type frame
type access
type frag = PROC of {body: Tree.stm; frame: frame} | STRING of Temp.label * string

val fp: Temp.temp
val rv: Temp.temp
val wordSize: int

val newFrame: name:Temp.label -> formals:bool list -> frame
val name: frame -> Temp.label
val formals: frame -> access list
val allocLocal: frame -> bool -> access
val exp: access * Tree.exp -> Tree.exp

end