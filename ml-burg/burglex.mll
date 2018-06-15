(* burg-lex
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * ML-Lex specification for ML-burg.
 *)
{
open Core
open Lexing

module T = struct
type ('a,'b) token = 
	| K_LPAREN of ('a *'b)
	| K_RPAREN of ('a *'b)
	| K_COMMA of ('a *'b)
	| K_COLON of ('a *'b)
	| K_SEMICOLON of ('a *'b)
	| K_EQUAL of ('a *'b)
	| K_PIPE of ('a *'b)
	| K_TERM of ('a *'b)
	| K_START of ('a *'b)
	| K_TERMPREFIX of ('a *'b)
	| K_RULEPREFIX of ('a *'b)
	| K_SIG of ('a *'b)
	| INT of (int * 'a *'b)
	| ID of (string * 'a *'b)
	| K_EOF of ('a *'b)
	| PPERCENT of ('a *'b)

type svalue = int
end

exception SyntaxError of string

type pos 		= int
type svalue		= T.svalue
type ('a,'b) token 	= ('a,'b) T.token
type lexresult		= (svalue, pos) token

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

let rawStop ()		= if !rawNoNewLine then rawNextLine () else ()

let eof ()		= (if !comLevel > 0 then raise (SyntaxError "unclosed comment")
			   else if !verbatimLevel <> 0 then
				   raise (SyntaxError  "unclosed user input")
			        else ();
			   if !reachedEop 
			   then T.K_EOF(!lineNum,!lineNum)
			   else	(rawStop ();
				 T.PPERCENT( (*rev(!raw),*) !lineNum,!lineNum)
				(* before (raw := [];
				        reachedEop := true) *)))
}

(* %s 			COMMENT DUMP POSTLUDE; *)

let idchars			= ['A'-'Z' 'a'-'z' '0'-'9' '_']
let id			= ['A'-'Z' 'a'-'z'] idchars*
let ws			= ['\t' ' ']*
let num			= ['0'-'9']+
let line			= _*


rule read =
	parse
	| '\n' { (inc lineNum; read lexbuf) }
	| "%{" { (incVerbLvl(); (* YYBEGIN DUMP; *) read lexbuf) }
	| "%%" { (inc percentCount; 
			    if !percentCount = 2 then 
					( (* YYBEGIN POSTLUDE; *) read lexbuf)
			    else 
					T.PPERCENT( (*rev(!raw),*) !lineNum,!lineNum)
					(*before raw := []*) ) }
	| ws { (read lexbuf) }
	| '\n' { (inc lineNum; read lexbuf) }
	| "(" { (T.K_LPAREN(!lineNum,!lineNum)) }
	| ")" { (T.K_RPAREN(!lineNum,!lineNum)) }
	| "," {(T.K_COMMA(!lineNum,!lineNum)) }
	| ":" { (T.K_COLON(!lineNum,!lineNum)) }
	| ";" { (T.K_SEMICOLON(!lineNum,!lineNum)) }
	| "=" { (T.K_EQUAL(!lineNum,!lineNum)) }
	| "|" { (T.K_PIPE(!lineNum,!lineNum)) }
	| "%term" { (T.K_TERM(!lineNum,!lineNum)) }
	| "%start" { (T.K_START(!lineNum,!lineNum)) }
	| "%termprefix" { (T.K_TERMPREFIX(!lineNum,!lineNum)) }
	| "%ruleprefix"	{ (T.K_RULEPREFIX(!lineNum,!lineNum)) }
	| "%sig" { (T.K_SIG(!lineNum,!lineNum)) }
	| "(*" { ( (*YYBEGIN COMMENT;*) comLevel:=1; read lexbuf) }
	| num { (T.INT( (*valOf*) (Int.of_string (Lexing.lexeme lexbuf)), !lineNum,!lineNum)) }
	| id { (T.ID((Lexing.lexeme lexbuf),!lineNum,!lineNum)) }

and read_comment =
	parse
	| "(*" { (inc comLevel; read lexbuf) }
	| '\n' { (inc lineNum; read lexbuf) }
	| "*)" { (dec comLevel;
					(* if !comLevel=0 then YYBEGIN INITIAL else (); *)
					read lexbuf) }
	| _	{ (read lexbuf) }

(*<DUMP> "%}"		=> (rawStop(); dec verbatimLevel;
			    YYBEGIN INITIAL; read lexbuf);
<DUMP> "\n"		=> (rawNextLine (); inc lineNum; read lexbuf);
<DUMP> {line}		=> (outputRaw yytext; read lexbuf);


<POSTLUDE> "\n"		=> (rawNextLine (); inc lineNum; read lexbuf);
<POSTLUDE> {line}	=> (outputRaw yytext; read lexbuf); *)
