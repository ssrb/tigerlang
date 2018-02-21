module Lexer = Tigerlex.F(Parsertokens)
module Translate = Translate.F(M68kFrame)
module Semant = Semant.F(Translate)
module Canon = Canon.F(M68kFrame.Tree)
module M68K = M68kCodegen
module Makegraph = Makegraph.F(M68kCodegen.Assem)
module Liveness = Liveness.F(Makegraph.Flow)
module Color = Color.F (M68kFrame) (Liveness)

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
			|> Makegraph.instrs2graph
		in 

		let (igraph, _) = Liveness.interferenceGraph graph in

		ignore(Color.color {interference = igraph; initial = M68kTemp.Table.empty; spillCost = (fun _ -> 0); registers = []});

		igraph::acc

	| Translate.Frame.STRING _ -> acc
in

let _ = fragments |> List.fold ~init:[] ~f:f in
flush stdout
