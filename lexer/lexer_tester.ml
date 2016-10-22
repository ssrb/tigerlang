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
	| OP_PLUS -> "OP_PLUS"
	| OP_MINUS -> "OP_MINUS"
	| OP_MUL -> "OP_MUL"
	| OP_DIV -> "OP_DIV"
	| OP_AND -> "OP_AND"
	| OP_OR -> "OP_OR"
	| OP_EQ -> "OP_EQ"
	| OP_NEQ -> "OP_NEQ"
	| OP_GT -> "OP_GT"
	| OP_GEQ -> "OP_GEQ"
	| OP_LT -> "OP_LT"
	| OP_LEQ -> "OP_LEQ"
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