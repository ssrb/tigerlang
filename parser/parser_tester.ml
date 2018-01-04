module Lexer = Tigerlex.F(Parsertokens)

let _ =
	let lexbuf = Lexing.from_channel stdin in
	Tigerparse.prog Lexer.read lexbuf