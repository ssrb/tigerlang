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

open Frame
open Temp

type exp = unit
type level = Outermost | Level of int * Frame.frame
type access = level * Frame.access
type nlparams = {parent: level; name: Temp.label; formals: bool list}

let outermost = Outermost

let newLevel {parent; name; formals} = 
let frame = Frame.newFrame {name; formals = true::formals} in
match parent with
| Outermost -> Level (0, frame)
| Level (depth, _) -> Level (succ depth, frame)

let formals lvl = []

let allocLocal lvl escape = 
match lvl with
| Outermost -> assert(false)
| Level (depth, frame) -> (lvl, Frame.allocLocal frame escape)

end