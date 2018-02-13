open Core

type unique = unit ref sexp_opaque
and ty = 
| RECORD of (Symbol.symbol * ty) list * unique
| NIL
| INT
| STRING
| ARRAY of ty * unique
| NAME of Symbol.symbol * ty option ref
| UNIT [@@deriving sexp]
