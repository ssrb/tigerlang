module type T = sig

module Temp: Temp.T

type label = Temp.label [@@deriving sexp]

type stm = 
| SEQ of stm * stm
| LABEL of label
| JUMP of exp * label list
| CJUMP of relop * exp * exp * label * label
| MOVE of exp * exp
| EXP of exp 
| NOP [@@deriving sexp]

and texp = 
| BINOP of binop * exp * exp
| MEM of exp
| TEMP of Temp.temp
| ESEQ of stm * exp
| NAME of label
| CONST of int
| CALL of exp * exp list [@@deriving sexp]

and exp = { t: texp; addr: bool } [@@deriving sexp]

and binop = PLUS | MINUS | MUL | DIV 
| AND | OR | LSHIFT | RSHIFT | ARSHIFT | XOR [@@deriving sexp]

and relop = EQ | NE | LT | GT | LE | GE 
| ULT | ULE | UGT | UGE [@@deriving sexp]

val notRel : relop -> relop
val commute: relop -> relop
val seq: stm list -> stm
end

module F  = functor(Temp: Temp.T) -> struct

open Core

module Temp  = Temp

type label = Temp.label [@@deriving sexp]

type stm = 
| SEQ of stm * stm
| LABEL of label
| JUMP of exp * label list
| CJUMP of relop * exp * exp * label * label
| MOVE of exp * exp
| EXP of exp 
| NOP [@@deriving sexp]

and texp = 
| BINOP of binop * exp * exp
| MEM of exp
| TEMP of Temp.temp
| ESEQ of stm * exp
| NAME of label
| CONST of int
| CALL of exp * exp list [@@deriving sexp]

and exp = { t: texp; addr: bool } [@@deriving sexp]

and binop = PLUS | MINUS | MUL | DIV 
| AND | OR | LSHIFT | RSHIFT | ARSHIFT | XOR [@@deriving sexp]

and relop = EQ | NE | LT | GT | LE | GE 
| ULT | ULE | UGT | UGE [@@deriving sexp]

let notRel = function
| EQ -> NE
| NE -> EQ
| LT -> GE
| GT -> LE
| LE -> GT
| GE -> LT 
| ULT -> UGE
| ULE -> UGT
| UGT -> ULE
| UGE -> ULT

let commute = function
| EQ -> EQ
| NE -> NE
| LT -> GT
| GT -> LT
| LE -> GE
| GE -> LE 
| ULT -> UGT
| ULE -> UGE
| UGT -> ULT
| UGE -> ULE

let seq stms =
    let rec aux stms res =
        match stms with
        | [] -> assert(false)
        | [ stm ] -> SEQ (res, stm)
        | stm::stms' -> aux stms' (SEQ (res, stm))
    in
    match stms with
    | [] -> assert(false)
    | [ stm ] -> stm
    | stm::stms' -> aux stms' stm

end
