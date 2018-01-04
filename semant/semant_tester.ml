module Lexer = Tigerlex.F(Parsertokens)

let print_type ty = ();;

let {Semant.exp = _; ty } =
	let lexbuf = Lexing.from_channel stdin in
	Semant.transProg (Tigerparse.prog Lexer.read lexbuf);;

print_type ty

