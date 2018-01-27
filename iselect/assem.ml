module F = functor(Temp: Temp.T) -> struct

type reg = string
type temp = Temp.temp
type label = Temp.label

type instr = 
	| OPER of {assem: string; dst: temp list; src: temp list; jump: label list option}
	| LABEL of {assem: string; lab: Temp.label}
	| MOVE of {assem: string; dst: temp; src: temp}

let format saytemp =
	let speak(assem,dst,src,jump) =
		let saylab = Symbol.name in
		let rec f = function
			| "`"::"s"::i::rest ->
		    (explode(saytemp(List.nth(src,ord i - ord "0"))) @ (f rest)
		  |  "`"::"d"::i::rest ->
		    (explode(saytemp(List.nth(dst,ord i - ord "0"))) @ (f rest)
		  | "`"::"j":: i:: rest ->
		    (explode(saylab(List.nth(jump,ord i - ord "0"))) @ (f rest)
		  | "`"::"`"::rest -> "`"::(f rest)
		  | "`"::_::rest -> ErrorMsg.impossible "bad Assem format"
		  | c::rest -> c::(f rest)
		  | [] -> []
	  in implode(f(explode assem))  
  in (fun x -> match x with
	 	| OPER {assem;dst;src;jump=NONE} -> speak(assem,dst,src,nil)
    | OPER {assem;dst;src;jump=SOME j} -> speak(assem,dst,src,j)
	  | LABEL {assem} -> assem
	  | MOVE {assem;dst;src} -> speak(assem,[dst],[src],nil)

end

