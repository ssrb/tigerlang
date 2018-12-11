module Lexer = Tigerlex.F(Parsertokens)
module Translate = Translate.F(M68kFrame)
module Semant = Semant.F(Translate)
module Canon = Canon.F(M68kFrame.Tree)
module M68K = M68kCodegen
module Regalloc = Regalloc.F(M68K)
module Temp = M68kFrame.Temp
module TT = Temp.Table

open Core


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

		let asm, allocation = Regalloc.alloc (M68kFrame.procEntryExit2 (proc.frame, asm), proc.frame) in

		let asm = M68kFrame.procEntryExit3 (proc.frame, asm) in
		let outchannel = Out_channel.stdout in
		Out_channel.print_endline ((Symbol.name (M68kFrame.name proc.frame)) ^ ":");
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
		Out_channel.print_endline ((Symbol.name lbl) ^ ": even");
		Out_channel.print_endline ("\tdc.l " ^ (str |> String.length |> Int.to_string));
		Out_channel.print_endline ("\tdc.b \"" ^ (String.escaped str) ^ "\"")
in

let _ = 
Out_channel.print_endline "_tigermain:";
Out_channel.print_endline "\tpublic _tigermain";
fragments |> List.iter ~f:f in
()
