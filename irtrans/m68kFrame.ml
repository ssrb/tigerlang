type frame = unit
type access = unit
(* val newFrame: {name: Temp.label; formals: bool list} -> frame*)
(*val name: frame -> Temp.label*)
let formals f = []
let allocLocal  f  escape = ()
