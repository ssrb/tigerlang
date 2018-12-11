open Core
open Out_channel

module MAIN = functor(M68K: Codegen.T) -> struct

module Lexer = Tigerlex.F(Parsertokens)
module Frame = M68K.Frame
module Translate = Translate.F(Frame)
module Semant = Semant.F(Translate)
module Canon = Canon.F(Frame.Tree)

let fragments = 
	let lexbuf = Lexing.from_channel  In_channel.stdin in
	Tigerparse.prog Lexer.read lexbuf
	|> Semant.transProg2

let f outchannel frag =
	let output_string_endl out str = 
		output_string out str;
		newline out
	in
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
		Frame.(
			output_string outchannel asm.prolog;

			asm.body |> List.iter ~f:(fun instr -> 
				let asm = instr |> M68K.Assem.format_hum (fun tmp ->
					Option.value ~default:(Temp.makestring tmp.temp) (Temp.Table.look (Frame.tempMap, tmp.temp))
				) in
				if not (String.is_empty asm) then
					output_string_endl outchannel asm
			);

			output_string outchannel asm.epilog
		)

	| Translate.Frame.STRING (lbl, str) -> 
		output_string_endl outchannel ((Symbol.name lbl) ^ ": even");
		output_string_endl outchannel ("\tdc.l " ^ (str |> String.length |> Int.to_string));
		output_string_endl outchannel ("\tdc.b \"" ^ (String.escaped str) ^ "\"")

let test () = 
	fragments |> List.iter ~f:(f stdout);
	flush stdout

end

let _ = 
	if (Array.length Sys.argv) > 1 then
		let module Main = MAIN(M68000.Generator) in Main.test ()
	else
		let module Main = MAIN(M68kCodegen) in Main.test ()
		

	
