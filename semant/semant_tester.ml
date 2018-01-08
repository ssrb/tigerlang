module Lexer = Tigerlex.F(Parsertokens)
module M68KSemant = Semant.F(Translate.F(M68kFrame))

let {M68KSemant.exp = _; ty } =
	let lexbuf = Lexing.from_channel stdin in
	M68KSemant.transProg (Tigerparse.prog Lexer.read lexbuf);;

Prtype.print stdout ty

