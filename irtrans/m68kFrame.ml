module Temp = M68kTemp
type frame = unit
type access = InFrame of int | InReg of Temp.temp
type nfparams = {name: Temp.label; formals: bool list} 
let newFrame {name; formals} = ()
let name frame = Temp.newlabel ()
let formals f = []
let allocLocal  (f: frame)  (escape: bool) = InFrame 0
