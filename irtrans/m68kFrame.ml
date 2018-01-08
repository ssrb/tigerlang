module Temp = M68kTemp
type frame = unit
type access = unit
type nfparams = {name: Temp.label; formals: bool list} 
let newFrame {name; formals} = ()
let name frame = Temp.newlabel ()
let formals f = []
let allocLocal  f  escape = ()
