module Lexer = Tigerlex.F(Parsertokens)

let print_type outstream ty =
 let say s =  output_string outstream s in

 let sayln s = (say s; say "\n") in

 let rec indent = function 
  | 0 -> ()
  | i -> (say " "; indent(i-1))
 in

 let open Types in
 match ty with 
 | RECORD (tys, unique) -> say "{"; say "}"
 | NIL -> say "nil"
 | INT -> say "int"
 | STRING -> say "string"
 | ARRAY(ty', unique) -> ()
 | NAME (sym, tyoptref) -> ()
 | UNIT -> say "unit"

let {Semant.exp = _; ty } =
	let lexbuf = Lexing.from_channel stdin in
	Semant.transProg (Tigerparse.prog Lexer.read lexbuf);;

print_type stdout ty

