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
	| TYPE
	| DOT
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
	| COMMA
	| COLON
	| SEMICOLON
	| COMMENT
	| EOF

exception SyntaxError of string

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1
    }
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
  | "type" { TYPE }
  | id { ID(Lexing.lexeme lexbuf) }
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
  | ';'      { SEMICOLON }
  | "/*"      { read_comment lexbuf}
  | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
  | eof      { EOF }

 and read_string buf =
  parse
  | '"'       { STRING (Buffer.contents buf) }
  | '\\' '/'  { Buffer.add_char buf '/'; read_string buf lexbuf }
  | '\\' '\\' { Buffer.add_char buf '\\'; read_string buf lexbuf }
  | '\\' 'b'  { Buffer.add_char buf '\b'; read_string buf lexbuf }
  | '\\' 'f'  { Buffer.add_char buf '\012'; read_string buf lexbuf }
  | '\\' 'n'  { Buffer.add_char buf '\n'; read_string buf lexbuf }
  | '\\' 'r'  { Buffer.add_char buf '\r'; read_string buf lexbuf }
  | '\\' 't'  { Buffer.add_char buf '\t'; read_string buf lexbuf }
  | [^ '"' '\\']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_string buf lexbuf
    }
  | _ { raise (SyntaxError ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
  | eof { raise (SyntaxError ("String is not terminated")) }

and read_comment =
	parse
	(* Tiger supports nested comments *)
	| "/*"  { ignore(read_comment lexbuf); read_comment lexbuf }
	| "*/" { COMMENT }
	| _ { read_comment lexbuf }
    | eof { raise (SyntaxError ("String is not terminated")) }