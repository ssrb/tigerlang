module Lexer = Tigerlex.F(Parsertokens)
module Translate = Translate.F(M68kFrame)
module Semant = Semant.F(Translate)
module Canon = Canon.F(M68kFrame.Tree)
module M68K = M68kCodegen
module Regalloc = Regalloc.F(M68kFrame)

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

		let res = proc.body 
			|> Canon.linearize 
			|> Canon.basicBlocks 
			|> Canon.traceSchedule 
			|> List.map ~f:(M68K.codegen proc.frame)
			|> List.map ~f:(fun asm -> Regalloc.alloc (asm, proc.frame))
		in 

		res::acc

	| Translate.Frame.STRING _ -> acc
in

let _ = fragments |> List.fold ~init:[] ~f:f in
()
