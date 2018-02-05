module F = functor (Translate : Translate.T) ->
struct
module Temp = Translate.Temp
type varentry = {access: Translate.access; ty : Types.ty}
type funentry = {level: Translate.level; label: Temp.label; formals : Types.ty list; result: Types.ty}
type enventry = VarEntry of varentry | FunEntry of funentry

let base_tenv = 
    let tenv = Symbol.enter (Symbol.empty, Symbol.symbol "int", Types.INT) in
    let tenv = Symbol.enter (tenv, Symbol.symbol "string", Types.STRING) in
    let tenv = Symbol.enter (tenv, Symbol.symbol "nil", Types.NIL) in
    let tenv = Symbol.enter (tenv, Symbol.symbol "unit", Types.UNIT) in
    tenv

let base_venv = 
    let venv = Symbol.enter (Symbol.empty, Symbol.symbol "print", FunEntry {level = Translate.outermost; label = Temp.newlabel(); formals = [ Types.STRING ]; result = Types.UNIT}) in
    let venv = Symbol.enter (venv, Symbol.symbol "flush", FunEntry {level = Translate.outermost; label = Temp.newlabel(); formals = []; result = Types.UNIT}) in
    let venv = Symbol.enter (venv, Symbol.symbol "getchar", FunEntry {level = Translate.outermost; label = Temp.newlabel(); formals = []; result = Types.STRING}) in
    let venv = Symbol.enter (venv, Symbol.symbol "ord", FunEntry {level = Translate.outermost; label = Temp.newlabel(); formals = [ Types.STRING ]; result = Types.INT}) in
    let venv = Symbol.enter (venv, Symbol.symbol "chr", FunEntry {level = Translate.outermost; label = Temp.newlabel(); formals = [ Types.INT ]; result = Types.STRING}) in
    let venv = Symbol.enter (venv, Symbol.symbol "size", FunEntry {level = Translate.outermost; label = Temp.newlabel(); formals = [ Types.STRING ]; result = Types.INT}) in
    let venv = Symbol.enter (venv, Symbol.symbol "substring", FunEntry {level = Translate.outermost; label = Temp.newlabel(); formals = [ Types.STRING; Types.INT; Types.INT ]; result = Types.STRING}) in
    let venv = Symbol.enter (venv, Symbol.symbol "concat", FunEntry {level = Translate.outermost; label = Temp.newlabel(); formals = [ Types.STRING; Types.STRING ]; result = Types.STRING}) in
    let venv = Symbol.enter (venv, Symbol.symbol "not", FunEntry {level = Translate.outermost; label = Temp.newlabel(); formals = [ Types.INT ]; result = Types.INT}) in
    let venv = Symbol.enter (venv, Symbol.symbol "exit", FunEntry {level = Translate.outermost; label = Temp.newlabel(); formals = [ Types.INT ]; result = Types.UNIT}) in
    venv

end