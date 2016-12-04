type pos = Lexing.position
and symbol = Symbol.symbol

type oper = PlusOp | MinusOp | MulOp | DivOp | EqOp | NeqOp | LtOp | LeOp | GtOp | GeOp

type field = { name: symbol; escape: bool ref; typ: symbol; pos: pos }

type ty = 
    | NameTy of symbol * pos
    | RecordTy of field list
    | ArrayTy of symbol * pos

type typedec = { name: symbol; ty: ty; pos: pos }

type var = 
    | SimpleVar of symbol * pos
    | FieldVar of var * symbol * pos
    | SubscriptVar of var * exp * pos
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
    | ArrayExp of arrayexp
and dec = 
    | FunctionDec of fundec list
    | VarDec of vardec
    | TypeDec of typedec list
and callexp = { func: symbol; args: exp list; pos: pos }
and opexp = { left: exp; oper: oper; right: exp; pos: pos }
and recordexp = {fields: (symbol * exp * pos) list; typ: symbol; pos: pos}
and assignexp = { var: var; exp: exp; pos: pos }
and ifexp = { test: exp; then': exp; else': exp option; pos: pos }
and whileexp = { test: exp; body: exp; pos: pos }
and forexp = { var: symbol; escape: bool ref; lo: exp; hi: exp; body: exp; pos: pos }
and letexp = { decs: dec list; body: exp; pos: pos }
and arrayexp = { typ: symbol; size: exp; init: exp; pos: pos }
and vardec = { name: symbol; escape: bool ref; typ: (symbol * pos) option; init: exp; pos: pos }
and fundec = { name: symbol; params: field list; result: (symbol * pos) option; body: exp; pos: pos }
