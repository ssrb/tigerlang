module Testlex = Tigerlex.F(Testtokens)

open Core

let _ =
	let lexbuf = Lexing.from_channel In_channel.stdin in
	while not lexbuf.Lexing.lex_eof_reached do
		lexbuf |> Testlex.read |> print_endline
	done