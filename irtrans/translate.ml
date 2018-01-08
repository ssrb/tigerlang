module type T = sig
module Temp : Temp.T
type exp = unit
type level
type access
type nlparams = {parent: level; name: Temp.label; formals: bool list}
val outermost: level
val newLevel: nlparams -> level
val formals: level -> access list
val allocLocal: level -> bool -> access
end

module F = functor(Frame : Frame.T) ->
struct
module Temp = Frame.Temp
type exp = unit
type level = unit
type access = unit
type nlparams = {parent: level; name: Temp.label; formals: bool list}
let outermost = ()
let newLevel ps = ()
let formals lvl = []
let allocLocal lvl escape = ()
end