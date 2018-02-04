module Lexer = Tigerlex.F(Parsertokens)
module Translate = Translate.F(M68kFrame)
module Semant = Semant.F(Translate)
module Canon = Canon.F(M68kFrame.Tree)
module M68K = M68kCodegen

open Core

let _ = 

let open Out_channel in

let fragments = 
	let lexbuf = Lexing.from_channel  In_channel.stdin in
	Tigerparse.prog Lexer.read lexbuf
	|> Semant.transProg2
in

let f frag =
	match frag with
	| Translate.Frame.PROC proc -> 
		proc.body 
		|> Canon.linearize 
		|> Canon.basicBlocks 
		|> Canon.traceSchedule 
		|> List.iter ~f:(fun tree -> 
			tree
			|> M68K.codegen proc.frame
			|> List.iter ~f:(fun asm -> 
				asm
				|> M68K.Assem.sexp_of_instr
				|> Sexp.output_hum stdout;
				newline stdout))
	| Translate.Frame.STRING _ -> ()
in

let _ = fragments |> List.iter ~f:f in
flush stdout
