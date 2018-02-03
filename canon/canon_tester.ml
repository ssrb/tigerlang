module Lexer = Tigerlex.F(Parsertokens)
module Translate = Translate.F(M68kFrame)
module Semant = Semant.F(Translate)
module Tree = Translate.Frame.Tree
module Canon = Canon.F(Tree)

open Core

let () = 

let fragments = 
	let lexbuf = Lexing.from_channel In_channel.stdin in
	Semant.transProg2 (Tigerparse.prog Lexer.read lexbuf)
in

let f frag =
	match frag with
	| Translate.Frame.PROC proc -> 
		let ts = proc.body |> Canon.linearize |> Canon.basicBlocks |> Canon.traceSchedule in
		List.iter ts ~f:(fun t -> Tree.sexp_of_stm t |> Sexp.output_hum Out_channel.stdout )
	| Translate.Frame.STRING (lab, str) -> (Tree.LABEL lab) |> Tree.sexp_of_stm |> Sexp.output_hum Out_channel.stdout
in

fragments |> List.iter ~f:f
