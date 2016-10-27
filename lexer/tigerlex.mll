{
include Tigerlex_intf
module F = functor (T: tokens) -> struct  
open Lexing
open T

exception SyntaxError of string

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1
    }

let ls = Lexing.lexeme_start
let le = Lexing.lexeme_end
}

(* 
	Appendix A.1 of the Tiger lang ref says an id is a sequence of letter, digit and undersore starting with a letter:
	so in Tiger, ids are not allowed to start with an underscore.
*)
let id = ['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

let digit = ['0'-'9']
(*
 Negative integers are parsed as two lexems: MINUS INT
*)
let int = digit digit*
let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

rule read =
  parse
  | white { read lexbuf }
  | newline { next_line lexbuf; read lexbuf }
  | int { _INT (int_of_string (Lexing.lexeme lexbuf), (ls lexbuf), (le lexbuf)) }
  | "nil" { _NIL ((ls lexbuf), (le lexbuf)) }
  | '"' { read_string (Buffer.create 17) lexbuf }
  | "let" { _LET ((ls lexbuf), (le lexbuf))}
  | "in" { _IN ((ls lexbuf), (le lexbuf))}
  | "end" { _END ((ls lexbuf), (le lexbuf))}
  | "var" { _VAR ((ls lexbuf), (le lexbuf))}
  | "function" { _FUNCTION ((ls lexbuf), (le lexbuf))}
  | "for" { _FOR ((ls lexbuf), (le lexbuf))}
  | "while" { _WHILE ((ls lexbuf), (le lexbuf))}
  | "do" { _DO ((ls lexbuf), (le lexbuf))}
  | "to" { _TO ((ls lexbuf), (le lexbuf))}
  | "break" { _BREAK ((ls lexbuf), (le lexbuf))}
  | "if" { _IF ((ls lexbuf), (le lexbuf))}
  | "then" { _THEN ((ls lexbuf), (le lexbuf))}
  | "else" { _ELSE ((ls lexbuf), (le lexbuf))}
  | "array" { _ARRAY ((ls lexbuf), (le lexbuf))}
  | "of" { _OF ((ls lexbuf), (le lexbuf))}
  | "type" { _TYPE ((ls lexbuf), (le lexbuf))}
  | id { _ID(Lexing.lexeme lexbuf, (ls lexbuf), (le lexbuf)) }
  | '.' { _DOT ((ls lexbuf), (le lexbuf))}
  | '+' { _PLUS ((ls lexbuf), (le lexbuf))}
  | '-' { _MINUS ((ls lexbuf), (le lexbuf))}
  | '*' { _MUL ((ls lexbuf), (le lexbuf))}
  | '/' { _DIV ((ls lexbuf), (le lexbuf))}
  | '&' { _AND ((ls lexbuf), (le lexbuf))}
  | '|' { _OR ((ls lexbuf), (le lexbuf))}
  | '=' { _EQ ((ls lexbuf), (le lexbuf))}
  | "<>" { _NEQ ((ls lexbuf), (le lexbuf))}
  | '>' { _GT ((ls lexbuf), (le lexbuf))}
  | ">=" { _GE ((ls lexbuf), (le lexbuf))}
  | '<' { _LT ((ls lexbuf), (le lexbuf))}
  | "<=" { _LE ((ls lexbuf), (le lexbuf))}
  | ":=" { _ASSIGN ((ls lexbuf), (le lexbuf))}
  | '(' { _LPAREN ((ls lexbuf), (le lexbuf))}
  | '{' { _LBRACE ((ls lexbuf), (le lexbuf))}
  | '[' { _LBRACK ((ls lexbuf), (le lexbuf))}
  | ')' { _RPAREN ((ls lexbuf), (le lexbuf))}
  | '}' { _RBRACE ((ls lexbuf), (le lexbuf))}
  | ']' { _RBRACK ((ls lexbuf), (le lexbuf))}
  | ':' { _COLON ((ls lexbuf), (le lexbuf))}
  | ',' { _COMMA ((ls lexbuf), (le lexbuf))}
  | ';' { _SEMICOLON ((ls lexbuf), (le lexbuf))}
  | "/*" { read_comment lexbuf}
  | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
  | eof { 
    lexbuf.lex_eof_reached <- true;
    _EOF ((ls lexbuf), (le lexbuf))
  }

and read_string buf =
  parse
  | '"' { _STRING (Buffer.contents buf, (ls lexbuf), (le lexbuf)) }
  | '\\' '/' { Buffer.add_char buf '/'; read_string buf lexbuf }
  | '\\' '\\' { Buffer.add_char buf '\\'; read_string buf lexbuf }
  | '\\' 'b' { Buffer.add_char buf '\b'; read_string buf lexbuf }
  | '\\' 'f' { Buffer.add_char buf '\012'; read_string buf lexbuf }
  | '\\' 'n' { Buffer.add_char buf '\n'; read_string buf lexbuf }
  | '\\' 'r' { Buffer.add_char buf '\r'; read_string buf lexbuf }
  | '\\' 't' { Buffer.add_char buf '\t'; read_string buf lexbuf }
  | [^ '"' '\\']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_string buf lexbuf
    }
  | _ { raise (SyntaxError ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
  | eof { raise (SyntaxError ("String is not terminated")) }

and read_comment =
	parse
	(* Tiger supports nested comments *)
	| "/*" { ignore(read_comment lexbuf); read_comment lexbuf }
	| "*/" { _COMMENT ((ls lexbuf), (le lexbuf)); read lexbuf}
	| _ { read_comment lexbuf }
  | eof { raise (SyntaxError ("String is not terminated")) }

{
  end;;
}