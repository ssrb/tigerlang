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
end

module F = functor(Frame : Frame.T) ->
struct
module Temp = Frame.Temp
module Tree = Tree.F(Temp)

open Core
open Frame
open Temp

type exp = Ex of Tree.exp | Nx of Tree.stm | Cx of (Tree.label * Tree.label -> Tree.stm)
type _level = {level: level; frame: Frame.frame}
and level = Outermost | Level of _level * unit ref
type access = level * Frame.access

let outermost = Outermost

let newLevel ~parent ~name ~formals =
Level ({level = parent; frame = Frame.newFrame ~name ~formals:(true::formals)}, ref ())

let formals lvl = 
match lvl with
| Outermost -> assert(false)
| Level ({frame; _}, _) -> frame |> Frame.formals |> List.tl_exn |> List.map ~f:(fun acc -> (lvl, acc))

let allocLocal lvl escape = 
match lvl with
| Outermost -> assert(false)
| Level ({frame; _}, _) -> (lvl, Frame.allocLocal frame escape)

end