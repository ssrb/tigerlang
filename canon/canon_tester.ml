module Lexer = Tigerlex.F(Parsertokens)
module M68KSemant = Semant.F(Translate.F(M68kFrame))
let ctrees = 
	let lexbuf = Lexing.from_channel stdin in
	M68KSemant.transProg2 (Tigerparse.prog Lexer.read lexbuf)
	|> List.map ~f:(fun t -> Canon.linearize |> Canon.basicBlocks |> Canon.traceSchedule)
