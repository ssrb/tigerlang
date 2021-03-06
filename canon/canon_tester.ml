module Lexer = Tigerlex.F(Parsertokens)
module Translate = Translate.F(M68kFrame)
module Semant = Semant.F(Translate)
module Tree = M68kFrame.Tree
module Canon = Canon.F(Tree)

open Core
open Out_channel

let _ = 

let fragments = 
	let lexbuf = Lexing.from_channel In_channel.stdin in
	Tigerparse.prog Lexer.read lexbuf
	|> Semant.transProg2 
in

let f frag =
	match frag with
	| Translate.Frame.PROC proc -> 
		let ts = proc.body |> Canon.linearize |> Canon.basicBlocks |> Canon.traceSchedule in
		Tree.LABEL (M68kFrame.name proc.frame) |> Tree.sexp_of_stm |> Sexp.output_hum stdout;
		newline stdout;
		List.iter ts ~f:(fun t ->
			Tree.sexp_of_stm t |> Sexp.output_hum stdout;
			newline stdout)
	| Translate.Frame.STRING (lab, str) -> 
		(Tree.LABEL lab) |> Tree.sexp_of_stm |> Sexp.output_hum stdout;
		newline stdout;
		print_endline ("\"" ^ str ^ "\"")
in

let _ = fragments |> List.iter ~f:f in 

flush stdout
