open Core

module Temp = M68kTemp

type access = InFrame of int | InReg of Temp.temp
type frame = {name: Temp.label; faccess: access list}
type nfparams = {name: Temp.label; formals: bool list} 

let newFrame {name; formals} = {name; faccess = formals |> List.map ~f:(fun f -> InFrame 0)}

let name frame = Temp.newlabel ()

let formals {faccess; _} = faccess

let allocLocal  (f: frame)  (escape: bool) = InFrame 0

