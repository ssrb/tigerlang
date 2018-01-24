module Lexer = Tigerlex.F(Parsertokens)
module Translate = Translate.F(M68kFrame)
module Semant = Semant.F(Translate)
module Canon = Canon.F(Translate.Frame.Tree)

open Core

let fragments = 
	let lexbuf = Lexing.from_channel stdin in
	Semant.transProg2 (Tigerparse.prog Lexer.read lexbuf);;

let f acc frag =
	match frag with
	| Translate.Frame.PROC proc -> 
		let body = proc.body |> Canon.linearize |> Canon.basicBlocks |> Canon.traceSchedule in
		(body, proc.frame)::acc
	| Translate.Frame.STRING _ -> acc;;

fragments |> List.fold ~init:[] ~f:f