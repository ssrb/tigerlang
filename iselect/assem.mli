module type T = sig

module Temp : Temp.T

exception Assem_error of string

type reg = string [@@deriving sexp]
type temp = Temp.temp [@@deriving sexp]
type label = Temp.label [@@deriving sexp]

type instr = 
	| OPER of {assem: string; dst: temp list; src: temp list; jump: label list option}
	| LABEL of {assem: string; lab: Temp.label}
	| MOVE of {assem: string; dst: temp; src: temp} [@@deriving sexp]

val format : (temp -> string) -> instr -> string
val format_hum : (temp -> string) -> instr -> string

end

module F : functor(Temp: Temp.T) -> T with module Temp = Temp