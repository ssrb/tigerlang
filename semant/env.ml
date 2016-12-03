(*type access =*)  

type varentry = {ty : Types.ty}
type funentry = {formals : Types.ty list; result: Types.ty}
type enventry = Varentry of varentry | FunEntry of funentry

let base_tenv = Symbol.empty
let base_venv = Symbol.empty