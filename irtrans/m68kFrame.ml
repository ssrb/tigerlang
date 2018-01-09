open Core

module Temp = M68kTemp

type access = InFrame of int | InReg of Temp.temp
type frame = {name: Temp.label; formals: access list}

let newFrame ~name ~formals = {name; formals = formals |> List.map ~f:(fun f -> InFrame 0)}

let name {name; _} = name

let formals {formals; _} = formals

let allocLocal  (f: frame)  (escape: bool) = InFrame 0
