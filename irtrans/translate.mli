module type T = sig
module Temp : Temp.T
module Tree: Tree.T

type exp = Ex of Tree.exp | Nx of Tree.stm | Cx of (Tree.label * Tree.label -> Tree.stm)
type level
type access

val outermost: level
val newLevel: parent:level -> name:Temp.label -> formals:bool list -> level
val formals: level -> access list
val allocLocal: level -> bool -> access

val unEx: exp -> Tree.exp
val unNx: exp -> Tree.stm
(*val unCx: exp -> Temp.label * Temp.label -> Tree.stm*)

end

module F : functor(Frame: Frame.T) -> T