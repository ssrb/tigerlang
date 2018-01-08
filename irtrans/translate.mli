module type T = sig
type exp = unit
type level
type access
type nlparams = {parent: level; name: Temp.label; formals: bool list}
val outermost: level
val newLevel: nlparams -> level
val formals: level -> access list
val allocLocal: level -> bool -> access
end

module F : T