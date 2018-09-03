open Core

module MAIN = functor(M68K: Codegen.T) -> struct

module Lexer = Tigerlex.F(Parsertokens)
module Frame = M68K.Frame
module Translate = Translate.F(Frame)
module Semant = Semant.F(Translate)
module Canon = Canon.F(Frame.Tree)

open Out_channel

let fragments = 
	let lexbuf = Lexing.from_channel  In_channel.stdin in
	Tigerparse.prog Lexer.read lexbuf
	|> Semant.transProg2

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
		let asm = Frame.procEntryExit2 (proc.frame, asm) in
		let asm = Frame.procEntryExit3 (proc.frame, asm) in
		let outchannel = stdout in
		Frame.(
			output_string outchannel asm.prolog;

			asm.body |> List.iter ~f:(fun instr -> 
				let asm = instr |> M68K.Assem.format_hum (fun tmp ->
					Option.value ~default:(Temp.makestring tmp.temp) (Temp.Table.look (Frame.tempMap, tmp.temp))
				) in
				if not (String.is_empty asm) then
				begin 
					output_string outchannel asm;
					newline outchannel
				end
			);

			output_string outchannel asm.epilog
		)

	| Translate.Frame.STRING (lbl, str) -> 
		print_endline ((Symbol.name lbl) ^ ": even");
		print_endline ("\tdc.l " ^ (str |> String.length |> Int.to_string));
		print_endline ("\tdc.b \"" ^ (String.escaped str) ^ "\"")

let test () = 
	fragments |> List.iter ~f:f;
	flush stdout

end

let _ = 
	if (Array.length Sys.argv) > 1 then
		let module Main = MAIN(M68000.Generator) in Main.test ()
	else
		let module Main = MAIN(M68kCodegen) in Main.test ()
		

	
