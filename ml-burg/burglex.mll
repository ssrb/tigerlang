(* burg-lex
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * ML-Lex specification for ML-burg.
 *)
{
open Core
open Lexing

module T = Burgparse

exception SyntaxError of string

type pos 		= int
type svalue		= int
type token	 	= T.token
type lexresult		= token

let comLevel		= ref 0
let lineNum		= ref 0
let verbatimLevel	= ref 0
let percentCount	= ref 0
let rawLine		= ref ""
let rawNoNewLine	= ref false
let raw:string list ref = ref []
let reachedEop		= ref false

let resetState ()	= (comLevel      := 0;
			   lineNum       := 0;
			   verbatimLevel := 0;
			   percentCount  := 0;
			   rawLine	 := "";
			   rawNoNewLine	 := false;
			   raw		 := [];
			   reachedEop	 := false)
			   
let inc ri = ri := !ri + 1
let dec ri = ri := !ri - 1

let incVerbLvl ()	= if !verbatimLevel <> 0 
			  then raise (SyntaxError "nested verbatim levels")
			  else inc verbatimLevel

let outputRaw (s:string) = (rawLine := !rawLine^s; rawNoNewLine := true)

let rawNextLine ()	= (raw := (!rawLine ^ "\n")::!raw;
			   rawLine := ""; rawNoNewLine := false)

let rawStop ()		= if !rawNoNewLine then rawNextLine ()

let eof ()		=
	if !comLevel > 0 then 
		raise (SyntaxError "unclosed comment")
	else if !verbatimLevel <> 0 then
		raise (SyntaxError  "unclosed user input");
	if !reachedEop then 
		T.K_EOF
	else (
		rawStop ();
		let t = T.PPERCENT( List.rev(!raw) ) in
		raw := [];
		reachedEop := true;
		t
	)
}

(* %s 			COMMENT DUMP POSTLUDE; *)

let idchars			= ['A'-'Z' 'a'-'z' '0'-'9' '_']
let id			= ['A'-'Z' 'a'-'z'] idchars*
let ws			= ['\t' ' ']*
let num			= ['0'-'9']+
let line			= _*


rule read =
	parse
	| '\n' { inc lineNum; read lexbuf }
	| "%{" { incVerbLvl(); Out_channel.print_endline "Dump"; dump lexbuf }
	| "%%" { inc percentCount; 
			    if !percentCount = 2 then 
					( (* YYBEGIN POSTLUDE; *) read lexbuf)
			    else (
					let t = T.PPERCENT( List.rev(!raw)) in
					raw := [];
					t
				) }
	| ws { read lexbuf }
	| '\n' { inc lineNum; read lexbuf }
	| "(" { T.K_LPAREN }
	| ")" { T.K_RPAREN }
	| "," { T.K_COMMA }
	| ":" { T.K_COLON }
	| ";" { T.K_SEMICOLON }
	| "=" { T.K_EQUAL }
	| "|" { T.K_PIPE }
	| "%term" { T.K_TERM }
	| "%start" { T.K_START }
	| "%termprefix" { T.K_TERMPREFIX }
	| "%ruleprefix"	{ T.K_RULEPREFIX }
	| "%sig" { T.K_SIG }
	| "(*" { ( (*YYBEGIN COMMENT;*) comLevel:=1; read lexbuf) }
	| num { (T.INT( (*valOf*) (Int.of_string (Lexing.lexeme lexbuf)))) }
	| id { (T.ID((Lexing.lexeme lexbuf))) }
	| eof { lexbuf.lex_eof_reached <- true; eof () }

and read_comment =
	parse
	| "(*" { (inc comLevel; read lexbuf) }
	| '\n' { (inc lineNum; read lexbuf) }
	| "*)" { (dec comLevel;
					(* if !comLevel=0 then YYBEGIN INITIAL else (); *)
					read lexbuf) }
	| _	{ (read lexbuf) }

and dump = 
	parse 
	| "%}" { rawStop(); dec verbatimLevel; (* YYBEGIN INITIAL; *) Out_channel.print_endline "End dump"; read lexbuf }
	| "\n" { rawNextLine (); inc lineNum; dump lexbuf }
	| _	{ outputRaw (Lexing.lexeme lexbuf); dump lexbuf }


(* <POSTLUDE> "\n"		=> (rawNextLine (); inc lineNum; read lexbuf);
<POSTLUDE> {line}	=> (outputRaw yytext; read lexbuf); *)
