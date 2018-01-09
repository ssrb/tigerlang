open Core

module Temp = M68kTemp

type access = InFrame of int | InReg of Temp.temp
type frame = {name: Temp.label; formals: access list; offset: int ref}

let (++) r inc = let x = !r in r := x + inc; x

let newFrame ~name ~formals = 
let off = ref 0 in
let formals = formals |> List.map ~f:(fun f -> InFrame (off ++ 4)) in
{name; formals; offset = off}

let name {name; _} = name

let formals {formals; _} = formals

let allocLocal f  escape = InFrame (f.offset ++ 4)
