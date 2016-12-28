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
      | Env.FunEntry {formals; result} -> (
        if (List.length args) <>  (List.length formals) then 
          raise (Semantic_error "missing argument in function call");
        List.zip_exn args formals |> List.iter ~f:(fun (a, ty) -> 
          let { exp = _; ty = ty' } = transExp (venv, tenv, a) in 
          if ty <> ty' then
            (raise (Semantic_error "wrong type in function call"))
        );
        {exp = (); ty = actual_ty result }
      ))
    | None -> raise (Semantic_error "unknown function name"))
  
  | A.OpExp {left; oper; right; pos} ->
    let {exp = _; ty = tyleft} = transExp (venv, tenv, left)
    and {exp = _; ty = tyright} = transExp (venv, tenv, right)
    in
    if tyleft = Types.INT && tyright = Types.INT then
        { exp = (); ty = Types.INT }
    else
        raise (Semantic_error "integer expected")
  
  | A.RecordExp {fields; typ; pos} ->
    (match Symbol.look (tenv, typ) with
    | Some rty -> (match rty with
      | Types.RECORD (ftypes, _) -> (
          List.iter fields ~f:(fun (sym, exp, pos) -> 
            let { exp = _; ty } = transExp (venv, tenv, exp) in 
            if not (List.exists ftypes (fun ft -> ft = (sym, ty))) then
              raise (Semantic_error "not a record type"));
          { exp = (); ty = rty }
      )
      | _ -> raise (Semantic_error "not a record type"))
    | None -> raise (Semantic_error "unknown record type"))

  | A.SeqExp l -> (
    if List.is_empty l then
      { exp = (); ty = Types.UNIT }
    else
      List.map l (fun e -> transExp (venv, tenv, fst e)) |> List.last_exn
  )

  | A.AssignExp {var = v; exp = e; pos} -> (
    let { exp = _; ty = tyleft} = transVar (venv, tenv, v)
    and { exp = _; ty = tyright} = transExp (venv, tenv, e)
    in
      if tyleft = tyright then 
        { exp = (); ty = tyleft }
      else
        raise (Semantic_error "Incompatible type in assignment")
  )

  | A.IfExp {test = test ; then'= then'; else'= else'; pos} -> (
    let { exp = _; ty = tytest} = transExp (venv, tenv, test)
    and { exp = _; ty = tythen} = transExp (venv, tenv, then')
    in
      if tytest = Types.INT then
        (match else' with
        | None -> { exp = (); ty = Types.NIL }
        | Some e -> 
          let { exp = _; ty = tyelse } = transExp (venv, tenv, e) in
          if tythen = tyelse then
            { exp = (); ty = tythen }
          else
            raise (Semantic_error "If-then-else type is inconsistent"))
      else
        raise (Semantic_error "If test must be of integer type");
    )

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