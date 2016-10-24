open Lexing
open Tigerlex
open Tigerparse

let _ =
	let lexbuf = Lexing.from_channel stdin in
	Tigerparse.prog Tigerlex.read lexbuf