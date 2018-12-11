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

(**
  * Direct translation of the example of glpk's reference manual.
  *
  * @author Samuel Mimram
  *)

(*open Glpk

let () =
  let lp = make_problem Maximize
             [|10.; 6.; 4.|]
             [|
               [|1.; 1.; 1.|];
               [|10.; 4.; 5.|];
               [|2.; 2.; 6.|]
             |]
             [| -.infinity, 100.; -.infinity, 600.; -.infinity, 300. |]
             [| 0., infinity; 0., infinity; 0., infinity|] in
    scale_problem lp;
    use_presolver lp true;
    simplex lp;
    let prim = get_col_primals lp in
      Printf.printf "Z: %g    x0: %g    x1: %g    x2: %g\n%!" (get_obj_val lp) prim.(0) prim.(1) prim.(2)*)

let _ = 

let fragments = 
	let lexbuf = Lexing.from_channel  In_channel.stdin in
	Tigerparse.prog Lexer.read lexbuf
	|> Semant.transProg2
in

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

let _ = 
print_endline "_tigermain:";
print_endline "\tpublic _tigermain";
fragments |> List.iter ~f:(f stdout) in
flush stdout
