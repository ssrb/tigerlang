module F : functor(Temp: Temp.T) -> sig

type reg = string
type temp = Temp.temp
type label = Temp.label

type instr = 
	| OPER of {assem: string; dst: temp list; src: temp list; jump: label list option}
	| LABEL of {assem: string; lab: Temp.label}
	| MOVE of {assem: string; dst: temp; src: temp}

val format : (temp -> string) -> instr -> string

end