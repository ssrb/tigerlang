module F = functor(Temp: Temp.T) -> struct

open Core

exception Assem_error of string

type reg = string
type temp = Temp.temp
type label = Temp.label

type instr = 
	| OPER of {assem: string; dst: temp list; src: temp list; jump: label list option}
	| LABEL of {assem: string; lab: Temp.label}
	| MOVE of {assem: string; dst: temp; src: temp}

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
	 	| OPER {assem;dst;src;jump=None} -> speak(assem, dst, src, [])
    | OPER {assem;dst;src;jump=Some j} -> speak(assem, dst, src,j)
	  | LABEL {assem} -> assem
	  | MOVE {assem;dst;src} -> speak(assem, [dst], [src], []))

end
