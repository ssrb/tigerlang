module Lexer = Tigerlex.F(Parsertokens)
module Translate = Translate.F(M68kFrame)
module Semant = Semant.F(Translate)
module Canon = Canon.F(Translate.Frame.Tree)
module M68K = M68kCodegen

open Core

let _ = 

let fragments = 
	let lexbuf = Lexing.from_channel  In_channel.stdin in
	Tigerparse.prog Lexer.read lexbuf
	|> Semant.transProg2
in

let f frag =
	match frag with
	| Translate.Frame.PROC proc -> 
		()
		(*proc.body 
		|> Canon.linearize 
		|> Canon.basicBlocks 
		|> Canon.traceSchedule 
		|> List.iter ~f:(fun tree -> 
			tree
			|> M68K.codegen proc.frame
			|> List.iter ~f:(fun asm -> 
				asm
				|> sexp_of_instr
				|> Sexp.output_hum Out_channel.stdout))*)
	| Translate.Frame.STRING _ -> ()
in

let _ = fragments |> List.iter ~f:f in
Out_channel.flush Out_channel.stdout
