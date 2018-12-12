module Lexer = Tigerlex.F(Parsertokens)
module Translate = Translate.F(M68kFrame)
module Semant = Semant.F(Translate)
module Canon = Canon.F(M68kFrame.Tree)
module M68K = M68kCodegen
module Regalloc = Regalloc.F(M68K)
module Temp = M68kFrame.Temp
module TT = Temp.Table

open Core
open Out_channel

let output_string_endl out str = 
		output_string out str;
		newline out
;;

let compile tigersource outputasm outfile = 

	let fragments = 
		In_channel.with_file tigersource ~f:(fun inchannel ->
			let lexbuf = Lexing.from_channel inchannel in
			Tigerparse.prog Lexer.read lexbuf
			|> Semant.transProg2
		)
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

			output_string_endl outchannel ((Symbol.name (M68kFrame.name proc.frame)) ^ ":");

			M68kFrame.(
				output_string outchannel asm.prolog;

				asm.body |> List.iter ~f:(fun instr ->
				
					let asm = instr |> M68K.Assem.format_hum (fun tmp ->
						Option.value ~default:(Temp.makestring tmp.temp) (TT.look (allocation, tmp.temp))
					) 
					in

					if not (String.is_empty asm) then
						output_string_endl outchannel asm
				);
				output_string outchannel asm.epilog;
			)
		
		| Translate.Frame.STRING (lbl, str) -> 
			output_string_endl outchannel ((Symbol.name lbl) ^ ": even");
			output_string_endl outchannel ("\tdc.l " ^ (str |> String.length |> Int.to_string));
			output_string_endl outchannel ("\tdc.b \"" ^ (String.escaped str) ^ "\"")
	in

	let asm = 
		if not outputasm then 
			Filename.temp_file  "tiger" ".S"
		else match outfile with
		| Some outfile -> outfile
		| None -> (tigersource |> Filename.basename |> Filename.chop_extension) ^ ".S"
	in

	Out_channel.with_file asm ~f:(fun outchannel ->
		output_string_endl outchannel "_tigermain:";
		output_string_endl outchannel "\tpublic _tigermain";
		fragments |> List.iter ~f:(f outchannel)
	);

	if not outputasm then
		let outfile = Option.value outfile ~default:"a.out" in
		let son = Unix.fork_exec ~prog:"vc" ~argv:[ "vc"; "+kick13"; "runtime.c"; asm; "-o"; outfile ] () in
		Unix.waitpid_exn son;
		Unix.remove asm
;;

let spec =
  let open Command.Spec in
  empty
  +> anon ("tigersource" %: file)
  +> flag "-S" no_arg ~doc:"Do not assemble.  The output is in the form of an assembler code file."
  +> flag "-o" (optional string) ~doc:"outfile The outputfile name"
;;

let command =
  Command.basic_spec
    ~summary:"ssrb's Tiger toy compiler"
    ~readme:(fun () -> "Compile a single Tiger source file to an AmigaOS(Kickstart ROM 1.3) loadseg()ble executable/binary")
    spec
    (fun tigersource outputasm outfile () -> compile tigersource outputasm outfile)
;;

let () = Command.run ~version:"0.1" ~build_info:"" command




