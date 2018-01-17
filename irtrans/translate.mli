module type T = sig

module Frame: Frame.T
module Temp : Temp.T

type exp
type level
type access

val outermost: level

val newLevel: parent:level -> name:Temp.label -> formals:bool list -> level
val formals: level -> access list
val allocLocal: level -> bool -> access
val getResult: unit -> Frame.frag list

val transNil: unit -> exp
val transInt: int -> exp
val transOp: Absyn.oper * exp * exp -> exp
val transSeq: exp list -> exp
val transString: string -> exp
val transCall: level * level * Temp.label * exp list * Types.ty -> exp
val transRecord: exp option list -> exp
val transAssign: exp * exp -> exp
val transIf: exp * exp * exp option -> exp
val transWhile: exp * exp * Temp.label -> exp
val transBreak: Temp.label -> exp
val transVar: access * level -> exp

val toDo: unit -> exp

end

module F : functor(Frame: Frame.T) -> T