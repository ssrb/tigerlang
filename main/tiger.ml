module Lexer = Tigerlex.F(Parsertokens)
module Translate = Translate.F(M68kFrame)
module Semant = Semant.F(Translate)
module Canon = Canon.F(M68kFrame.Tree)
module M68K = M68kCodegen
module Regalloc = Regalloc.F(M68K)
module Temp = M68kFrame.Temp
module TT = Temp.Table

open Core

let _ = 

let open Out_channel in

let fragments = 
	let lexbuf = Lexing.from_channel  (In_channel.create Sys.argv.(1)) in
	Tigerparse.prog Lexer.read lexbuf
	|> Semant.transProg2
in

let f outchannel frag =
	match frag with
	| Translate.Frame.PROC proc -> 

		let asm = proc.body 
			|> Canon.linearize 
			|> Canon.basicBlocks 
			|> Canon.traceSchedule 
			|> List.map ~f:(M68K.codegen proc.frame)
			|> List.concat
		in 

		let asm, allocation = Regalloc.alloc (M68kFrame.procEntryExit2 (proc.frame, asm), proc.frame) in

		let asm = M68kFrame.procEntryExit3 (proc.frame, asm) in

		Out_channel.output_string outchannel ((Symbol.name (M68kFrame.name proc.frame)) ^ ":");
		Out_channel.newline outchannel;

		M68kFrame.(
			Out_channel.output_string outchannel asm.prolog;

			asm.body |> List.iter ~f:(fun instr ->
			
				let asm = instr |> M68K.Assem.format_hum (fun tmp ->
					Option.value ~default:(Temp.makestring tmp.temp) (TT.look (allocation, tmp.temp))
				) 
				in

				if not (String.is_empty asm) then
				begin 
					Out_channel.output_string outchannel asm;
					Out_channel.newline outchannel
				end
			);
			Out_channel.output_string outchannel asm.epilog;
		)
	
	| Translate.Frame.STRING (lbl, str) -> 
		Out_channel.output_string outchannel ((Symbol.name lbl) ^ ": even");
		Out_channel.newline outchannel;
		Out_channel.output_string outchannel ("\tdc.l " ^ (str |> String.length |> Int.to_string));
		Out_channel.newline outchannel;
		Out_channel.output_string outchannel ("\tdc.b \"" ^ (String.escaped str) ^ "\"");
		Out_channel.newline outchannel
in

let outchannel = Out_channel.stdout in
Out_channel.output_string outchannel "_tigermain:";
Out_channel.newline outchannel;
Out_channel.output_string outchannel "\tpublic _tigermain";
Out_channel.newline outchannel;
fragments |> List.iter ~f:(f outchannel);
let son = Unix.fork_exec ~prog:"vc" ~argv:[ "vc"; "+kick13" ] () in
Unix.waitpid_exn son
