(*type access*)

type varentry = {ty : Types.ty}
type funentry = {formals : Types.ty list; result: Types.ty}
type enventry = VarEntry of varentry | FunEntry of funentry

val base_tenv : Types.ty Symbol.table
val base_venv : enventry Symbol.table
