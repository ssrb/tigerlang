(* main.sml
 *
 * COPYRIGHT (c) 1994 AT&T Bell Laboratories.
 *
 * $Log$
 * Revision 1.4  2001/11/21 21:03:16  blume
 * Release 110.37 -- see HISTORY
 *
 * Revision 1.3.4.1  2001/11/17 03:14:16  blume
 * fixed uses of exnMessage in standalone programs
 *
 * Revision 1.3  2000/06/01 18:33:42  monnier
 * bring revisions from the vendor branch to the trunk
 *
 * Revision 1.2  2000/03/07 03:59:09  blume
 * build script now uses new mechanism for building stanalone programs
 *
 * Revision 1.1.1.8.4.1  2000/02/20 14:44:33  blume
 * CMB.deliver merged with CMB.make; runtime boot code made more flexible
 *
 * Revision 1.1.1.8  1999/04/17 18:56:04  monnier
 * version 110.16
 *
 * Revision 1.1.1.1  1997/01/14 01:37:59  george
 *   Version 109.24
 *
 * Revision 1.1.1.2  1997/01/11  18:52:31  george
 *   ml-burg Version 109.24
 *
 * Revision 1.3  1996/02/26  16:55:12  jhr
 * Moved exportFn/exportML to SMLofNJ structure.
 *
 * Revision 1.2  1996/02/26  15:02:06  george
 *    print no longer overloaded.
 *    use of makestring has been removed and replaced with Int.toString ..
 *    use of IO replaced with TextIO
 *
 * Revision 1.1.1.1  1996/01/31  16:01:25  george
 * Version 109
 * 
 *)

open Core

let main (cmdName, argv) = 
	let help () = Out_channel.(
		output_string stderr "usage: mlburg [<filename>.burg]\n";
		exit (-1)
	)
	in

	match argv with
	| [] ->
		Burg.emit (In_channel.stdin, (fun () -> Out_channel.stdout));
		exit 0
	| "-h"::_ -> help ()
	| "-help"::_ -> help ()
	| files -> 
		let findname file = 
			let (base, ext) = Filename.split_extension file in
			match ext with 
			| Some ("brg" | "burg") ->
				base ^ ".sml"
			| _ -> 
				file ^ ".sml"
		in
		let names = List.map ~f:(fun n -> (n, findname n)) files in
		let emit (inname, outname) = 
			let s_in = In_channel.create inname in
			try
				Burg.emit (s_in, (fun () -> (Out_channel.create outname)))
			with
			| err -> 
				Out_channel.(output_string stderr ((Exn.to_string err) ^ "\n"));
				raise err
		in
		
		List.iter ~f:emit names;
		exit 0;;

main (Sys.argv.(0), List.tl_exn (Array.to_list Sys.argv))

  (*
   * This is the function to call in an interactive session.
   * Takes a filename (something.burg) as argument, and produces something.sml
   *)
(*    fun doit s = main ("", [s]) *)
