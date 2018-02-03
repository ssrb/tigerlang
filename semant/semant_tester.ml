module Lexer = Tigerlex.F(Parsertokens)
module Semant = Semant.F(Translate.F(M68kFrame))

open Semant
open Core

let expty =
	let lexbuf = Lexing.from_channel In_channel.stdin in
	Semant.transProg (Tigerparse.prog Lexer.read lexbuf);;

Prtype.print stdout expty.ty

