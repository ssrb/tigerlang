module Lexer = Tigerlex.F(Parsertokens)
module Translate = Translate.F(M68kFrame)
module Semant = Semant.F(Translate)
module Tree = Translate.Frame.Tree
module Canon = Canon.F(Tree)
module PrintTree = Printtree.F(Tree)

open Core

let fragments = 
	let lexbuf = Lexing.from_channel stdin in
	Semant.transProg2 (Tigerparse.prog Lexer.read lexbuf);;

let f frag =
	match frag with
	| Translate.Frame.PROC proc -> 
		let ts = proc.body |> Canon.linearize |> Canon.basicBlocks |> Canon.traceSchedule in
		List.iter ts ~f:(fun t -> PrintTree.printtree Out_channel.stdout t; Out_channel.print_endline "#")
	| Translate.Frame.STRING (lab, str) -> PrintTree.printtree Out_channel.stdout (Tree.LABEL lab); Out_channel.print_endline ("\"" ^ str ^ "\""); Out_channel.print_endline "#";;

fragments |> List.iter ~f:f