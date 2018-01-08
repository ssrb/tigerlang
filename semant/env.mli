module F : functor (Translate : Translate.T) ->
sig
type varentry = {access: Translate.access; ty : Types.ty}
type funentry = {level: Translate.level; label: Temp.label; formals : Types.ty list; result: Types.ty}
type enventry = VarEntry of varentry | FunEntry of funentry

val base_tenv : Types.ty Symbol.table
val base_venv : enventry Symbol.table
end