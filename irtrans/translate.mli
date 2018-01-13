module type T = sig

module Temp : Temp.T

type exp
type level
type access

val outermost: level
val newLevel: parent:level -> name:Temp.label -> formals:bool list -> level
val formals: level -> access list
val allocLocal: level -> bool -> access

val transNil: unit -> exp
val transInt: int -> exp
val transOp: Absyn.oper * exp * exp -> exp
val transVar: access * level -> exp
val transSeq: exp list -> exp

val toDo: unit -> exp

end

module F : functor(Frame: Frame.T) -> T