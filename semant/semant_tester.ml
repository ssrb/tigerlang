module Lexer = Tigerlex.F(Parsertokens)
module Semant = Semant.F(Translate.F(M68kFrame))

open Semant
open Core

let _ = 
let expty =
	let lexbuf = Lexing.from_channel In_channel.stdin in
	Semant.transProg (Tigerparse.prog Lexer.read lexbuf)
in
expty.ty |> Types.sexp_of_ty |> Sexp.output_hum Out_channel.stdout

