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
    let declareBuiltin ~name ~formals ~result venv = 
        Symbol.enter (venv, Symbol.symbol name, FunEntry {level = Translate.outermost; label = Temp.namedlabel name; formals; result})
    in
    Symbol.empty
    |> declareBuiltin ~name:"print" ~formals:[ Types.STRING ] ~result:Types.UNIT
    |> declareBuiltin ~name:"flush" ~formals:[] ~result:Types.UNIT
    |> declareBuiltin ~name:"getchar" ~formals:[] ~result:Types.STRING
    |> declareBuiltin ~name:"ord" ~formals:[ Types.STRING] ~result:Types.INT
    |> declareBuiltin ~name:"chr" ~formals:[ Types.INT ] ~result:Types.STRING
    |> declareBuiltin ~name:"size" ~formals:[ Types.STRING ] ~result:Types.INT
    |> declareBuiltin ~name:"substring" ~formals:[ Types.STRING; Types.INT; Types.INT ] ~result:Types.STRING
    |> declareBuiltin ~name:"concat" ~formals:[ Types.STRING; Types.STRING ] ~result:Types.STRING
    |> declareBuiltin ~name:"not" ~formals:[ Types.INT ] ~result:Types.INT
    |> declareBuiltin ~name:"exit" ~formals:[ Types.INT ] ~result:Types.UNIT

end