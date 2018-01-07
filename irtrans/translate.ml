type exp = unit
type level = unit
type access = unit
type nlparams = {parent: level; name: Temp.label; formals: bool list}
let outermost = ()
let newLevel ps = ()
let formals lvl = []
let allocLocal lvl escape = ()