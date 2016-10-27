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
  | int { _INT (int_of_string (Lexing.lexeme lexbuf), (Lexing.lexeme_start lexbuf), (Lexing.lexeme_end lexbuf)) }
  | "nil" { _NIL ((Lexing.lexeme_start lexbuf), (Lexing.lexeme_end lexbuf)) }
  | '"' { read_string (Buffer.create 17) lexbuf }
  | "let" { _LET ((Lexing.lexeme_start lexbuf), (Lexing.lexeme_end lexbuf))}
  | "in" { _IN ((Lexing.lexeme_start lexbuf), (Lexing.lexeme_end lexbuf))}
  | "end" { _END ((Lexing.lexeme_start lexbuf), (Lexing.lexeme_end lexbuf))}
  | "var" { _VAR ((Lexing.lexeme_start lexbuf), (Lexing.lexeme_end lexbuf))}
  | "function" { _FUNCTION ((Lexing.lexeme_start lexbuf), (Lexing.lexeme_end lexbuf))}
  | "for" { _FOR ((Lexing.lexeme_start lexbuf), (Lexing.lexeme_end lexbuf))}
  | "while" { _WHILE ((Lexing.lexeme_start lexbuf), (Lexing.lexeme_end lexbuf))}
  | "do" { _DO ((Lexing.lexeme_start lexbuf), (Lexing.lexeme_end lexbuf))}
  | "to" { _TO ((Lexing.lexeme_start lexbuf), (Lexing.lexeme_end lexbuf))}
  | "break" { _BREAK ((Lexing.lexeme_start lexbuf), (Lexing.lexeme_end lexbuf))}
  | "if" { _IF ((Lexing.lexeme_start lexbuf), (Lexing.lexeme_end lexbuf))}
  | "then" { _THEN ((Lexing.lexeme_start lexbuf), (Lexing.lexeme_end lexbuf))}
  | "else" { _ELSE ((Lexing.lexeme_start lexbuf), (Lexing.lexeme_end lexbuf))}
  | "array" { _ARRAY ((Lexing.lexeme_start lexbuf), (Lexing.lexeme_end lexbuf))}
  | "of" { _OF ((Lexing.lexeme_start lexbuf), (Lexing.lexeme_end lexbuf))}
  | "type" { _TYPE ((Lexing.lexeme_start lexbuf), (Lexing.lexeme_end lexbuf))}
  | id { _ID(Lexing.lexeme lexbuf, (Lexing.lexeme_start lexbuf), (Lexing.lexeme_end lexbuf)) }
  | '.' { _DOT ((Lexing.lexeme_start lexbuf), (Lexing.lexeme_end lexbuf))}
  | '+' { _PLUS ((Lexing.lexeme_start lexbuf), (Lexing.lexeme_end lexbuf))}
  | '-' { _MINUS ((Lexing.lexeme_start lexbuf), (Lexing.lexeme_end lexbuf))}
  | '*' { _MUL ((Lexing.lexeme_start lexbuf), (Lexing.lexeme_end lexbuf))}
  | '/' { _DIV ((Lexing.lexeme_start lexbuf), (Lexing.lexeme_end lexbuf))}
  | '&' { _AND ((Lexing.lexeme_start lexbuf), (Lexing.lexeme_end lexbuf))}
  | '|' { _OR ((Lexing.lexeme_start lexbuf), (Lexing.lexeme_end lexbuf))}
  | '=' { _EQ ((Lexing.lexeme_start lexbuf), (Lexing.lexeme_end lexbuf))}
  | "<>" { _NEQ ((Lexing.lexeme_start lexbuf), (Lexing.lexeme_end lexbuf))}
  | '>' { _GT ((Lexing.lexeme_start lexbuf), (Lexing.lexeme_end lexbuf))}
  | ">=" { _GE ((Lexing.lexeme_start lexbuf), (Lexing.lexeme_end lexbuf))}
  | '<' { _LT ((Lexing.lexeme_start lexbuf), (Lexing.lexeme_end lexbuf))}
  | "<=" { _LE ((Lexing.lexeme_start lexbuf), (Lexing.lexeme_end lexbuf))}
  | ":=" { _ASSIGN ((Lexing.lexeme_start lexbuf), (Lexing.lexeme_end lexbuf))}
  | '(' { _LPAREN ((Lexing.lexeme_start lexbuf), (Lexing.lexeme_end lexbuf))}
  | '{' { _LBRACE ((Lexing.lexeme_start lexbuf), (Lexing.lexeme_end lexbuf))}
  | '[' { _LBRACK ((Lexing.lexeme_start lexbuf), (Lexing.lexeme_end lexbuf))}
  | ')' { _RPAREN ((Lexing.lexeme_start lexbuf), (Lexing.lexeme_end lexbuf))}
  | '}' { _RBRACE ((Lexing.lexeme_start lexbuf), (Lexing.lexeme_end lexbuf))}
  | ']' { _RBRACK ((Lexing.lexeme_start lexbuf), (Lexing.lexeme_end lexbuf))}
  | ':' { _COLON ((Lexing.lexeme_start lexbuf), (Lexing.lexeme_end lexbuf))}
  | ',' { _COMMA ((Lexing.lexeme_start lexbuf), (Lexing.lexeme_end lexbuf))}
  | ';' { _SEMICOLON ((Lexing.lexeme_start lexbuf), (Lexing.lexeme_end lexbuf))}
  | "/*" { read_comment lexbuf}
  | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
  | eof { 
    lexbuf.lex_eof_reached <- true;
    _EOF ((Lexing.lexeme_start lexbuf), (Lexing.lexeme_end lexbuf))
  }

and read_string buf =
  parse
  | '"' { _STRING (Buffer.contents buf, (Lexing.lexeme_start lexbuf), (Lexing.lexeme_end lexbuf)) }
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
	| "*/" { _COMMENT ((Lexing.lexeme_start lexbuf), (Lexing.lexeme_end lexbuf))}
	| _ { read_comment lexbuf }
  | eof { raise (SyntaxError ("String is not terminated")) }

{
  end;;
}