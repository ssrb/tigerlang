open Core.Std

module A = Absyn

exception Semantic_error of string

type venv = Env.enventry Symbol.table
type tenv = Types.ty Symbol.table

type expty = {exp: Translate.exp; ty: Types.ty}

let actual_ty ty = ty

let rec transExp (venv, tenv, exp) = 
  match exp with
  | A.VarExp v -> transVar (venv, tenv, v)
  | A.NilExp -> { exp = (); ty = Types.NIL }
  | A.IntExp i -> { exp = (); ty = Types.INT }
  | A.StringExp (s,p) ->  { exp = (); ty = Types.STRING }
  | A.CallExp {func; args; pos} ->
    (match Symbol.look (venv, func) with
    | Some(entry) -> 
      (match entry with
      | Env.Varentry _ -> raise (Semantic_error "symbol is not a function")
      | Env.FunEntry {formals; result} -> { exp = (); ty = Types.STRING })
    | None -> raise (Semantic_error "unknown function name"))
  | A.OpExp {left; oper; right; pos} ->
    let {exp = _; ty = tyleft} = transExp (venv, tenv, left)
    and {exp = _; ty = tyright} = transExp (venv, tenv, right)
    in
    if tyleft = Types.INT && tyright = Types.INT then
        { exp = (); ty = Types.INT }
    else
        raise (Semantic_error "integer expectec")
  | A.RecordExp {fields; typ; pos} -> { exp = (); ty = Types.NIL } 
  | A.SeqExp l -> { exp = (); ty = Types.NIL }
  | A.AssignExp {var = v; exp = e; pos} -> { exp = (); ty = Types.NIL }
  | A.IfExp {test; then'; else'; pos} ->
      (match else' with
      | None -> { exp = (); ty = Types.NIL }
      | Some e -> { exp = (); ty = Types.NIL })
  | A.WhileExp {test; body; pos} -> { exp = (); ty = Types.NIL }
  | A.ForExp {var = v; escape = b; lo; hi; body; pos} -> { exp = (); ty = Types.NIL }
  | A.BreakExp p -> { exp = (); ty = Types.NIL }
  | A.LetExp {decs; body; pos} -> { exp = (); ty = Types.NIL }
  | A.ArrayExp {typ; size; init; pos} -> { exp = (); ty = Types.NIL }

and transVar (venv, tenv, var) =
  match var with 
  | A.SimpleVar (symbol, pos) -> 
    (match Symbol.look (venv, symbol) with
    | Some(entry) -> 
      (match entry with
      | Env.Varentry {ty} ->  { exp = (); ty = actual_ty ty }
      | Env.FunEntry {formals; result} -> raise (Semantic_error "function is not value"))
    | None -> raise (Semantic_error "unknown variable name"))
  | A.FieldVar (var, sym, pos) -> 
    (let {exp; ty} = transVar (venv, tenv, var) in
      match ty with
      | RECORD (fields, _) -> 
        (match List.find fields (fun (sym', _) -> sym == sym')  with
        | Some (_, ty) -> { exp = (); ty = actual_ty ty }
        | None -> raise (Semantic_error "unknown field for record")) 
      | _ -> raise (Semantic_error "var isn't a record"))
  | A.SubscriptVar (var, exp, pos) -> 
    (let {exp; ty} = transVar (venv, tenv, var) in
      match ty with
      | ARRAY (ty, _) -> { exp = (); ty = actual_ty ty }
      | _ -> raise (Semantic_error "subscripted is not an array"))

let transProg e0 = transExp (Env.base_venv, Env.base_tenv, e0)
(*val transExp: venv * tenv * Absyn.exp -> expty
val transDec: venv * tenv * Absyn.dec -> { venv: venv, tenv: tenv}
val transTy: tenv * Absyn.ty -> Types.ty




*)