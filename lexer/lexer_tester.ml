open Lexing
open Tigerlex
module Testlex = Tigerlex(Testtokens)
let _ =
	let lexbuf = Lexing.from_channel stdin in
	while not lexbuf.lex_eof_reached do
		lexbuf |> Testlex.read |> print_endline
	done