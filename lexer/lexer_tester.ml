module Testlex = Tigerlex.F(Testtokens)

let _ =
	let lexbuf = Lexing.from_channel stdin in
	while not lexbuf.Lexing.lex_eof_reached do
		lexbuf |> Testlex.read |> print_endline
	done