module type T = sig
module Temp : Temp.T
type exp = unit
type level
type access
val outermost: level
val newLevel: parent:level -> name:Temp.label -> formals:bool list -> level
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
type level = Outermost | Level of {level: level; frame: Frame.frame}
type access = level * Frame.access

let outermost = Outermost

let newLevel ~parent ~name ~formals =
Level {level = parent; frame = Frame.newFrame ~name ~formals:(true::formals)}

let formals lvl = 
match lvl with
| Outermost -> assert(false)
| Level {frame; _} -> frame |> Frame.formals |> List.tl_exn |> List.map ~f:(fun acc -> (lvl, acc))

let allocLocal lvl escape = 
match lvl with
| Outermost -> assert(false)
| Level {frame; _} -> (lvl, Frame.allocLocal frame escape)

end