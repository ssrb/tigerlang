module Lexer = Tigerlex.F(Parsertokens)

let {Semant.exp = _; ty } =
	let lexbuf = Lexing.from_channel stdin in
	Semant.transProg (Tigerparse.prog Lexer.read lexbuf);;

Prtype.print stdout ty
