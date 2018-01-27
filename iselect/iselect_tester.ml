module Lexer = Tigerlex.F(Parsertokens)
module Translate = Translate.F(M68kFrame)
module Semant = Semant.F(Translate)
module Canon = Canon.F(Translate.Frame.Tree)
module M68K = M68kCodegen

open Core

let fragments = 
	let lexbuf = Lexing.from_channel stdin in
	Semant.transProg2 (Tigerparse.prog Lexer.read lexbuf);;

let f acc frag =
	match frag with
	| Translate.Frame.PROC proc -> 
		let body = proc.body |> Canon.linearize |> Canon.basicBlocks |> Canon.traceSchedule (*|> M68K.codegen proc.frame*) in
		(body, proc.frame)::acc
	| Translate.Frame.STRING _ -> acc;;

fragments |> List.fold ~init:[] ~f:f

(* val instrs =   List.concat(map (Mips.codegen frame) stms') 
val format0 = Assem.format(Temp.makestring)
in  app (fn i => TextIO.output(out,format0 i)) instrs;
end
end
| emitproc out (F.STRING(lab,s)) = TextIO.output(out,F.string(lab,s))

fun withOpenFile fname f = 
let val out = TextIO.openOut fname
in (f out before TextIO.closeOut out) 
handle e => (TextIO.closeOut out; raise e)
end 

fun compile filename = 
let val absyn = Parse.parse filename
val frags = (FindEscape.prog absyn; Semant.transProg absyn)
in 
withOpenFile (filename ^ ".s") 
(fn out => (app (emitproc out) frags))
end
end*)



