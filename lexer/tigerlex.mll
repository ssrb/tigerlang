{

module type Tokens = sig
  type linenum = int
  type token
  val _TYPE:  linenum * linenum -> token
  val _VAR:  linenum * linenum -> token
  val _FUNCTION:  linenum * linenum -> token
  val _BREAK:  linenum * linenum -> token
  val _OF:  linenum * linenum -> token
  val _END:  linenum * linenum -> token
  val _IN:  linenum * linenum -> token
  val _NIL:  linenum * linenum -> token
  val _LET:  linenum * linenum -> token
  val _DO:  linenum * linenum -> token
  val _TO:  linenum * linenum -> token
  val _FOR:  linenum * linenum -> token
  val _WHILE:  linenum * linenum -> token
  val _ELSE:  linenum * linenum -> token
  val _THEN:  linenum * linenum -> token
  val _IF:  linenum * linenum -> token
  val _ARRAY:  linenum * linenum -> token
  val _ASSIGN:  linenum * linenum -> token
  val _OR:  linenum * linenum -> token
  val _AND:  linenum * linenum -> token
  val _GE:  linenum * linenum -> token
  val _GT:  linenum * linenum -> token
  val _LE:  linenum * linenum -> token
  val _LT:  linenum * linenum -> token
  val _NEQ:  linenum * linenum -> token
  val _EQ:  linenum * linenum -> token
  val _DIV:  linenum * linenum -> token
  val _MUL:  linenum * linenum -> token
  val _MINUS:  linenum * linenum -> token
  val _PLUS:  linenum * linenum -> token
  val _DOT:  linenum * linenum -> token
  val _RBRACE:  linenum * linenum -> token
  val _LBRACE:  linenum * linenum -> token
  val _RBRACK:  linenum * linenum -> token
  val _LBRACK:  linenum * linenum -> token
  val _RPAREN:  linenum * linenum -> token
  val _LPAREN:  linenum * linenum -> token
  val _SEMICOLON:  linenum * linenum -> token
  val _COLON:  linenum * linenum -> token
  val _COMMA:  linenum * linenum -> token
  val _STRING: (string) *  linenum * linenum -> token
  val _INT: int *  linenum * linenum -> token
  val _ID: string *  linenum * linenum -> token
  val _EOF:  linenum * linenum -> token
  val _COMMENT:  linenum * linenum -> token
end

module F = functor (T: Tokens) -> struct  
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
  | int { _INT (int_of_string (Lexing.lexeme lexbuf), 0, 0) }
  | "nil" { _NIL (0, 0) }
  | '"' { read_string (Buffer.create 17) lexbuf }
  | "let" { _LET (0, 0)}
  | "in" { _IN (0, 0)}
  | "end" { _END (0, 0)}
  | "var" { _VAR (0, 0)}
  | "function" { _FUNCTION (0, 0)}
  | "for" { _FOR (0, 0)}
  | "while" { _WHILE (0, 0)}
  | "do" { _DO (0, 0)}
  | "to" { _TO (0, 0)}
  | "break" { _BREAK (0, 0)}
  | "if" { _IF (0, 0)}
  | "then" { _THEN (0, 0)}
  | "else" { _ELSE (0, 0)}
  | "array" { _ARRAY (0, 0)}
  | "of" { _OF (0, 0)}
  | "type" { _TYPE (0, 0)}
  | id { _ID(Lexing.lexeme lexbuf, 0, 0) }
  | '.' { _DOT (0, 0)}
  | '+' { _PLUS (0, 0)}
  | '-' { _MINUS (0, 0)}
  | '*' { _MUL (0, 0)}
  | '/' { _DIV (0, 0)}
  | '&' { _AND (0, 0)}
  | '|' { _OR (0, 0)}
  | '=' { _EQ (0, 0)}
  | "<>" { _NEQ (0, 0)}
  | '>' { _GT (0, 0)}
  | ">=" { _GE (0, 0)}
  | '<' { _LT (0, 0)}
  | "<=" { _LE (0, 0)}
  | ":=" { _ASSIGN (0, 0)}
  | '(' { _LPAREN (0, 0)}
  | '{' { _LBRACE (0, 0)}
  | '[' { _LBRACK (0, 0)}
  | ')' { _RPAREN (0, 0)}
  | '}' { _RBRACE (0, 0)}
  | ']' { _RBRACK (0, 0)}
  | ':' { _COLON (0, 0)}
  | ',' { _COMMA (0, 0)}
  | ';' { _SEMICOLON (0, 0)}
  | "/*" { read_comment lexbuf}
  | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
  | eof { 
    lexbuf.lex_eof_reached <- true;
    _EOF (0, 0)
  }

and read_string buf =
  parse
  | '"' { _STRING (Buffer.contents buf, 0, 0) }
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
	| "*/" { _COMMENT (0, 0)}
	| _ { read_comment lexbuf }
  | eof { raise (SyntaxError ("String is not terminated")) }

{
  end;;
}