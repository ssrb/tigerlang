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

module F = functor(Temp: Temp.T) -> struct

open Core

exception Assem_error of string

module Temp = Temp

type reg = string [@@deriving sexp]
type temp = Temp.temp [@@deriving sexp]
type label = Temp.label [@@deriving sexp]

type instr = 
	| OPER of {assem: string; dst: temp list; src: temp list; jump: label list option}
	| LABEL of {assem: string; lab: Temp.label}
	| MOVE of {assem: string; dst: temp; src: temp} [@@deriving sexp]

let explode s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) [];;

let implode l =
  let res = String.create (List.length l) in
  let rec imp i = function
  | [] -> res
  | c :: l -> res.[i] <- c; imp (i + 1) l in
  imp 0 l;;

let format saytemp =
	let speak(assem, dst, src, jump) =
		let saylab = Symbol.name in
		let rec f = function
			| '`'::'s'::i::rest -> (explode(saytemp(List.nth_exn src ((int_of_char i) - (int_of_char '0'))))) @ (f rest)
		  |  '`'::'d'::i::rest -> (explode(saytemp(List.nth_exn dst ((int_of_char i) - (int_of_char '0'))))) @ (f rest)
		  | '`'::'j':: i:: rest -> (explode(saylab(List.nth_exn jump ((int_of_char i) - (int_of_char '0'))))) @ (f rest)
		  | '`'::'`'::rest -> '`'::(f rest)
		  | '`'::_::rest -> raise (Assem_error "bad Assem format")
		  | c::rest -> c::(f rest)
		  | [] -> []
	  in implode(f(explode assem))
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
