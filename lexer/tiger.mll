{
open Lexing

(* Tempprary until work on parser begins *)
type token = 
	| ID of (string)
	| INT of (int)
	| STRING of (string)
	| NIL
	| LET
	| IN
	| END
	| VAR
	| FUNCTION
	| IF
	| THEN
	| ELSE
	| FOR
	| WHILE
	| DO
	| TO
	| BREAK
	| ARRAY
	| OF
	| DOT
	| OP_EQ
	| OP_PLUS
	| OP_MINUS
	| OP_MUL
	| OP_DIV
	| OP_AND
	| OP_OR
	| OP_EQ
	| OP_NEQ
	| OP_GT
	| OP_GEQ
	| OP_LT
	| OP_LEQ
	| ASSIGN
	| RIGHT_PAREN
	| RIGHT_BRACK
	| RIGHT_BRACE
	| LEFT_PAREN
	| LEFT_BRACK
	| LEFT_BRACE
	| EOF
	| COMMA
	| COLON

type pos = int
}

(* 
	Appendix A.1 of the Tiger lang ref says an id is a sequence of letter, digit and undersore starting with a letter:
	so in Tiger, ids are not allowed to start with an underscore.
*)
let id = ['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

let digit = ['0'-'9']
let int = '-'? digit digit*

(* Tiger does not support floating point arithmetic *)
(*
	let frac = '.' digit*
	let exp = ['e' 'E'] ['-' '+']? digit+
	let float = digit* frac? exp?
*)

let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

rule read =
  parse
  | white    { read lexbuf }
  | newline  { next_line lexbuf; read lexbuf }
  | int      { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | "nil"   { NIL }
  | '"'      { read_string (Buffer.create 17) lexbuf }
  | "let" { LET }
  | "in" { IN }
  | "end" { END }
  | "var" { VAR }
  | "function" { FUNCTION }
  | "for" { FOR }
  | "while" { WHILE }
  | "do" { DO }
  | "to" { TO }
  | "array" { ARRAY }
  | "of" { OF }
  | '.' { DOT }
  | '+' { OP_PLUS }
  | '-' { OP_MINUS }
  | '*' { OP_MUL }
  | '/' { OP_DIV }
  | '&' { OP_AND }
  | '|' { OP_OR }
  | '=' { OP_EQ }
  | "<>" { OP_NEQ }
  | '>' { OP_GT }
  | ">=" { OP_GEQ }
  | '<' { OP_LT }
  | "<=" { OP_LEQ }
  | ":=" { ASSIGN }
  | '(' { LEFT_PAREN }
  | '{'      { LEFT_BRACE }
  | '['      { LEFT_BRACK }
  | ')' { RIGHT_PAREN }
  | '}'      { RIGHT_BRACE }
  | ']'      { RIGHT_BRACK }
  | ':'      { COLON }
  | ','      { COMMA }
  | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
  | eof      { EOF }