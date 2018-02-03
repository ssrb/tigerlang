open Core
type pos = Lexing.position sexp_opaque
and symbol = Symbol.symbol [@@deriving sexp]

type oper = PlusOp | MinusOp | MulOp | DivOp | EqOp | NeqOp | LtOp | LeOp | GtOp | GeOp [@@deriving sexp]

type field = { name: symbol; escape: bool ref; typ: symbol; pos: pos } [@@deriving sexp]

type ty = 
    | NameTy of symbol * pos
    | RecordTy of field list
    | ArrayTy of symbol * pos [@@deriving sexp]

type typedec = { name: symbol; ty: ty; pos: pos } [@@deriving sexp]

type var = 
    | SimpleVar of symbol * pos
    | FieldVar of var * symbol * pos
    | SubscriptVar of var * exp * pos [@@deriving sexp]
and exp = 
    | VarExp of var
    | NilExp
    | IntExp of int
    | StringExp of string * pos
    | CallExp of callexp
    | OpExp of opexp
    | RecordExp of recordexp
    | SeqExp of (exp * pos) list
    | AssignExp of assignexp
    | IfExp of ifexp
    | WhileExp of whileexp
    | ForExp of forexp
    | BreakExp of pos
    | LetExp of letexp
    | ArrayExp of arrayexp [@@deriving sexp]
and dec = 
    | FunctionDec of fundec list
    | VarDec of vardec
    | TypeDec of typedec list [@@deriving sexp]
and callexp = { func: symbol; args: exp list; pos: pos } [@@deriving sexp]
and opexp = { left: exp; oper: oper; right: exp; pos: pos } [@@deriving sexp]
and recordexp = {fields: (symbol * exp * pos) list; typ: symbol; pos: pos} [@@deriving sexp]
and assignexp = { var: var; exp: exp; pos: pos } [@@deriving sexp]
and ifexp = { test: exp; then': exp; else': exp option; pos: pos } [@@deriving sexp]
and whileexp = { test: exp; body: exp; pos: pos } [@@deriving sexp]
and forexp = { var: symbol; escape: bool ref; lo: exp; hi: exp; body: exp; pos: pos } [@@deriving sexp]
and letexp = { decs: dec list; body: exp; pos: pos } [@@deriving sexp]
and arrayexp = { typ: symbol; size: exp; init: exp; pos: pos } [@@deriving sexp]
and vardec = { name: symbol; escape: bool ref; typ: (symbol * pos) option; init: exp; pos: pos } [@@deriving sexp]
and fundec = { name: symbol; params: field list; result: (symbol * pos) option; body: exp; pos: pos } [@@deriving sexp]

