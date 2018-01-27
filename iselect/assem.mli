module type T = sig

module Temp : Temp.T

exception Assem_error of string

type reg = string
type temp = Temp.temp
type label = Temp.label

type instr = 
	| OPER of {assem: string; dst: temp list; src: temp list; jump: label list option}
	| LABEL of {assem: string; lab: Temp.label}
	| MOVE of {assem: string; dst: temp; src: temp}

val format : (temp -> string) -> instr -> string

end

module F : functor(Temp: Temp.T) -> T