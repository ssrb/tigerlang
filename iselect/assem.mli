module type T = sig

exception Assem_error of string

module Temp : Temp.T

module Variable : sig
	type t = { temp: Temp.temp; regclass: string } [@@deriving sexp]
	val make: Temp.temp * string -> t
	include Core.Comparable.S with type t := t
end

type label = Temp.label [@@deriving sexp]

type instr = 
	| OPER of {assem: string; dst: Variable.t list; src: Variable.t list; jump: label list option}
	| LABEL of {assem: string; lab: Temp.label}
	| MOVE of {assem: string; dst: Variable.t; src: Variable.t} [@@deriving sexp]

val format : (Variable.t -> string) -> instr -> string
val format_hum : (Variable.t -> string) -> instr -> string

end

module F : functor(Temp: Temp.T) -> T with module Temp = Temp