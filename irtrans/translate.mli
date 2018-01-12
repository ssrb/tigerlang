module type T = sig
module Temp : Temp.T
module Tree: Tree.T

type exp
type level
type access

val outermost: level
val newLevel: parent:level -> name:Temp.label -> formals:bool list -> level
val formals: level -> access list
val allocLocal: level -> bool -> access

val transVar: access * level -> exp
val toDo: unit -> exp

end

module F : functor(Frame: Frame.T) -> T