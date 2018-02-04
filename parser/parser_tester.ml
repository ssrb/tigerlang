module Lexer = Tigerlex.F(Parsertokens)

open Core

let _ =
	let lexbuf = Lexing.from_channel In_channel.stdin in
	let _ = Tigerparse.prog Lexer.read lexbuf |> Absyn.sexp_of_exp |> Sexp.output_hum Out_channel.stdout in
	Out_channel.flush Out_channel.stdout