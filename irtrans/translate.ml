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

open Core
open Frame
open Temp

type exp = unit
type level = int * Frame.frame
type access = level * Frame.access
type nlparams = {parent: level; name: Temp.label; formals: bool list}

let outermost = (0, Frame.newFrame {name = Temp.newlabel (); formals = [ true ]})

let newLevel {parent = (depth, _); name; formals} = 
let frame = Frame.newFrame {name; formals = true::formals} in (succ depth, frame)

let formals ((depth, frame) as lvl) = frame |> Frame.formals |> List.tl_exn |> List.map ~f:(fun acc -> (lvl, acc))

let allocLocal ((depth, frame) as lvl) escape = (lvl, Frame.allocLocal frame escape)

end