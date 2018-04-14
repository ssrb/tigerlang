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

module F = functor(Temp: Temp.T) -> struct

open Core

exception Assem_error of string

module Temp = Temp

module Variable = struct
	type t = { temp: Temp.temp; regclass: string } [@@deriving sexp]
	let make (t, c) = { temp = t; regclass = c }
	let compare left right = compare left.temp right.temp
	include Core.Comparable.Make (
		struct
			type _t = t [@@deriving sexp, compare]
			type t = _t [@@deriving sexp, compare] 
		end
	)
end


type label = Temp.label [@@deriving sexp]

type instr = 
	| OPER of {assem: string; dst: Variable.t list; src: Variable.t list; jump: label list option}
	| LABEL of {assem: string; lab: Temp.label}
	| MOVE of {assem: string; dst: Variable.t; src: Variable.t} [@@deriving sexp]

let atoi c = (int_of_char c) - (int_of_char '0')

let format saytemp =
	let speak(assem, dst, src, jump) =
		let saylab = Symbol.name in
		let rec f = function
			| '`'::'s'::c::rest -> (String.to_list (saytemp (List.nth_exn src (atoi c)))) @ (f rest)
			| '`'::'d'::c::rest -> (String.to_list (saytemp (List.nth_exn dst (atoi c)))) @ (f rest)
			| '`'::'j'::c:: rest -> (String.to_list (saylab (List.nth_exn jump (atoi c)))) @ (f rest)
			| '`'::'`'::rest -> '`'::(f rest)
			| '`'::_::rest -> raise (Assem_error "bad Assem format")
			| c::rest -> c::(f rest)
			| [] -> []
	  in 
	  assem 
	  |> String.to_list 
	  |> f 
	  |> String.of_char_list

	in (fun x -> match x with
	 	| LABEL lbl -> lbl.assem
		| OPER op -> speak(op.assem, op.dst, op.src, Option.value op.jump ~default:[])
		| MOVE mv -> speak(mv.assem, [mv.dst], [mv.src], []))

let format_hum saytemp x =
		(match x with
	 	| LABEL lbl -> ""
	 	| _ -> "\t") ^
		(format saytemp x)

end
