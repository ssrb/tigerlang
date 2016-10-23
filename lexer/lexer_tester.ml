open Lexing
open Tigerlex

let string_of_token  = function
  	| ID(_) -> "ID"
	| INT(_) -> "INT"
	| STRING(_) -> "STRING"
	| NIL -> "NIL"
	| LET -> "LET"
	| IN -> "IN"
	| END -> "END"
	| VAR -> "VAR"
	| FUNCTION -> "FUNCTION"
	| IF -> "IF"
	| THEN -> "THEN"
	| ELSE -> "ELSE"
	| FOR -> "FOR"
	| WHILE -> "WHILE"
	| DO -> "DO"
	| TO -> "TO"
	| TYPE -> "TYPE"
	| BREAK -> "BREAK"
	| ARRAY -> "ARRAY"
	| OF -> "OF"
	| DOT -> "DOT"
	| PLUS -> "OP_PLUS"
	| MINUS -> "OP_MINUS"
	| MUL -> "OP_MUL"
	| DIV -> "OP_DIV"
	| AND -> "OP_AND"
	| OR -> "OP_OR"
	| EQ -> "OP_EQ"
	| NEQ -> "OP_NEQ"
	| GT -> "OP_GT"
	| GEQ -> "OP_GEQ"
	| LT -> "OP_LT"
	| LEQ -> "OP_LEQ"
	| ASSIGN -> "ASSIGN"
	| RPAREN -> "RPAREN"
	| RBRACK -> "RBRACK"
	| RBRACE -> "RBRACE"
	| LPAREN -> "LPAREN"
	| LBRACK -> "LBRACK"
	| LBRACE -> "LBRACE"
	| EOF -> "EOF"
	| COMMA -> "COMMA"
	| COLON -> "COLON"
	| SEMICOLON -> "SEMICOLON"
	| COMMENT -> "COMMENT"
;;

let _ =
	let lexbuf = Lexing.from_channel stdin in
	while true do
	let token = Tigerlex.read lexbuf in
	match token with
	| Tigerlex.EOF -> exit 0
	| _ -> token |> string_of_token |> print_endline
	done