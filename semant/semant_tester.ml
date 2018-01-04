open Lexing
module Lexer = Tigerlex.F(Parsertokens)

let _ =
	let lexbuf = Lexing.from_channel stdin in
	Semant.transProg (Tigerparse.prog Lexer.read lexbuf)