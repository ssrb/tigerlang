module Lexer = Tigerlex.F(Parsertokens)

open Core

let _ =
	let lexbuf = Lexing.from_channel In_channel.stdin in
	Tigerparse.prog Lexer.read lexbuf |> Absyn.sexp_of_exp |> Sexp.output_hum Out_channel.stdout