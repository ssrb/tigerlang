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
		let asm = proc.body 
			|> Canon.linearize 
			|> Canon.basicBlocks 
			|> Canon.traceSchedule 
			|> List.map ~f:(M68K.codegen proc.frame)
			|> List.concat
		in
		let asm = M68kFrame.procEntryExit2 (proc.frame, asm) in
		let asm = M68kFrame.procEntryExit3 (proc.frame, asm) in

		M68kFrame.(
			Out_channel.print_endline asm.prolog;

			asm.body |> List.iter ~f:(fun instr -> 
				instr
				|> M68K.Assem.format_hum (fun v -> M68kTemp.makestring v.temp)
				|> Out_channel.print_endline
			);

			Out_channel.print_endline asm.epilog
		)

	| Translate.Frame.STRING (lbl, str) -> 
		Out_channel.print_endline ((Symbol.name lbl) ^ ": dc.b \"" ^ (String.escaped str) ^ "\"");

in

let _ = fragments |> List.iter ~f:f in
flush stdout
