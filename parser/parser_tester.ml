module Lexer = Tigerlex.F(Parsertokens)

open Core

let _ =
	let lexbuf = Lexing.from_channel In_channel.stdin in
	Prabsyn.print stdout (Tigerparse.prog Lexer.read lexbuf)