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

let percentCount	= ref 0
let rawLine		= ref ""
let rawNoNewLine	= ref false
let raw:string list ref = ref []

let resetState ()	= (
		percentCount  := 0;
		rawLine	 := "";
		rawNoNewLine	 := false;
		raw		 := [];
	)

let outputRaw (s:string) = (rawLine := !rawLine^s; rawNoNewLine := true)

let rawNextLine () = 
	raw := (!rawLine ^ "\n")::!raw;
	rawLine := ""; rawNoNewLine := false

let rawStop () = if !rawNoNewLine then rawNextLine ()

}

let idchars		= ['A'-'Z' 'a'-'z' '0'-'9' '_']
let id			= ['A'-'Z' 'a'-'z'] idchars*
let ws			= ['\t' ' ']*
let num			= ['0'-'9']+
let line		= _*

rule read =
	parse
	| '\n' { read lexbuf }
	| "%{" { verbatim lexbuf; read lexbuf }
	| "%%" { 
		percentCount := succ !percentCount;
		if !percentCount = 2 then ( 
			postlude lexbuf
		) else (
			let t = T.PPERCENT (List.rev !raw) in
			raw := [];
			t
		)
	}
	| ws { read lexbuf }
	| '\n' { read lexbuf }
	| '(' { T.LPAREN }
	| ')' { T.RPAREN }
	| ',' { T.COMMA }
	| ':' { T.COLON }
	| ';' { T.SEMICOLON }
	| '=' { T.EQUAL }
	| '|' { T.PIPE }
	| "%term" { T.TERM }
	| "%start" { T.START }
	| "%termprefix" { T.TERMPREFIX }
	| "%ruleprefix"	{ T.RULEPREFIX }
	| "%sig" { T.SIG }
	| "(*" { comment lexbuf; read lexbuf }
	| num { T.INT (Int.of_string (Lexing.lexeme lexbuf)) }
	| id { T.ID (Lexing.lexeme lexbuf) }
	| eof { 
		lexbuf.lex_eof_reached <- true;
		T.EOF
	}

and comment =
	parse
	| "(*" { comment lexbuf; comment lexbuf }
	| '\n' { comment lexbuf }
	| "*)" { () }
	| eof { raise (SyntaxError "Unclosed comment") }
	| _	{ comment lexbuf }

and verbatim = 
	parse 
	| "%{" { raise (SyntaxError "nested verbatim levels") }
	| '\n' { rawNextLine (); verbatim lexbuf }
	| "%}" { rawStop () }
	| eof { raise (SyntaxError  "Unclosed verbatim") }
	| _	{ outputRaw (Lexing.lexeme lexbuf); verbatim lexbuf }

and postlude =
	parse
	| '\n' { rawNextLine (); postlude lexbuf }
	| eof { 
		rawStop ();
		let t = T.PPERCENT (List.rev !raw) in
		raw := [];
		t
	}
	| _ { outputRaw (Lexing.lexeme lexbuf); postlude lexbuf }
