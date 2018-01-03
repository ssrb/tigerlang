(*type access =*)  

type varentry = {ty : Types.ty}
type funentry = {formals : Types.ty list; result: Types.ty}
type enventry = VarEntry of varentry | FunEntry of funentry

let base_tenv = 
    let tenv = Symbol.enter (Symbol.empty, Symbol.symbol "int", Types.INT) in
    let tenv = Symbol.enter (tenv, Symbol.symbol "string", Types.STRING) in
    let tenv = Symbol.enter (tenv, Symbol.symbol "nil", Types.NIL) in
    let tenv = Symbol.enter (tenv, Symbol.symbol "unit", Types.UNIT) in
    tenv

let base_venv = Symbol.empty