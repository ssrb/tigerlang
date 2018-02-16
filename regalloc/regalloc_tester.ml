module Lexer = Tigerlex.F(Parsertokens)
module Translate = Translate.F(M68kFrame)
module Semant = Semant.F(Translate)
module Canon = Canon.F(M68kFrame.Tree)
module M68K = M68kCodegen
module Flowgraph = Makegraph.F(M68kCodegen.Assem)
module Liveness = Liveness.F(Flowgraph.Flow)

open Core

let _ = 

let open Out_channel in

let fragments = 
	let lexbuf = Lexing.from_channel  In_channel.stdin in
	Tigerparse.prog Lexer.read lexbuf
	|> Semant.transProg2
in

let f acc frag =
	match frag with
	| Translate.Frame.PROC proc -> 
		let graph, _ = proc.body 
			|> Canon.linearize 
			|> Canon.basicBlocks 
			|> Canon.traceSchedule 
			|> List.fold 
				~init:[] 
				~f:(fun asm tree -> asm @ (M68K.codegen proc.frame tree))
			|> Flowgraph.instrs2graph
		in 
		(Liveness.interferenceGraph graph)::acc
	| Translate.Frame.STRING _ -> acc
in

let _ = fragments |> List.fold ~init:[] ~f:f in
flush stdout
