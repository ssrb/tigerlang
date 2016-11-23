type access
type ty

type varentry = {ty : ty}
type funentry = {formals : ty list; result: ty}
type enventry = Varentry of varentry | FunEntry of funentry

val base_tenv : ty Symbol.table
val base_venv : enventry Symbol.table
