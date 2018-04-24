module type T = sig
module Temp: Temp.T

type label = Temp.label [@@deriving sexp]

type stm = 
| SEQ of stm * stm
| LABEL of label
| JUMP of exp * label list
| CJUMP of relop * exp * exp * label * label
| MOVE of exp * exp
| EXP of exp [@@deriving sexp]

and exp = 
| BINOP of binop * exp * exp
| MEM of exp
| TEMP of { temp : Temp.temp; ptr: bool }
| ESEQ of stm * exp
| NAME of label
| CONST of int
| CALL of exp * exp list [@@deriving sexp]

and binop = PLUS | MINUS | MUL | DIV 
| AND | OR | LSHIFT | RSHIFT | ARSHIFT | XOR [@@deriving sexp]

and relop = EQ | NE | LT | GT | LE | GE 
| ULT | ULE | UGT | UGE [@@deriving sexp]

val notRel : relop -> relop
val commute: relop -> relop
val seq: stm list -> stm
end

module F : functor(Temp: Temp.T) -> T with module Temp = Temp
