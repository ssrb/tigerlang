open Core.Std

module A = Absyn
module S = Symbol
module E = Env
module T = Types

exception Semantic_error of string

type venv = Env.enventry Symbol.table
type tenv = Types.ty Symbol.table

type expty = {exp: Translate.exp; ty: Types.ty}

let actual_ty ty = ty

let rec transExp (venv, tenv, exp) = 
  let open A in
  match exp with
  | VarExp v -> transVar (venv, tenv, v)
  | NilExp -> { exp = (); ty = Types.NIL }
  | IntExp i -> { exp = (); ty = Types.INT }
  | StringExp (s,p) ->  { exp = (); ty = Types.STRING }
  
  | CallExp {func; args; pos} ->
    (match Symbol.look (venv, func) with
    | Some(entry) -> 
      let open Env in
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
  
  | OpExp {left; oper; right; pos} ->
    let {exp = _; ty = tyleft} = transExp (venv, tenv, left)
    and {exp = _; ty = tyright} = transExp (venv, tenv, right)
    in
    if tyleft = Types.INT && tyright = Types.INT then
        { exp = (); ty = Types.INT }
    else
        raise (Semantic_error "integer expected")
  
  | RecordExp {fields; typ; pos} ->
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

  | SeqExp l -> (
    if List.is_empty l then
      { exp = (); ty = Types.UNIT }
    else
      List.map l (fun e -> transExp (venv, tenv, fst e)) |> List.last_exn
  )

  | AssignExp {var; exp; pos} -> (
    let { exp = _; ty = tyleft} = transVar (venv, tenv, var)
    and { exp = _; ty = tyright} = transExp (venv, tenv, exp)
    in
      if tyleft = tyright then 
        { exp = (); ty = tyleft }
      else
        raise (Semantic_error "Incompatible type in assignment")
  )

  | IfExp {test; then'; else'; pos} -> (
    let { exp = _; ty = tytest } = transExp (venv, tenv, test)
    and { exp = _; ty = tythen } = transExp (venv, tenv, then')
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
        raise (Semantic_error "If test must be of integer type")
    )

  | WhileExp {test; body; pos} -> (
    let { exp = _; ty = tytest } = transExp (venv, tenv, test)
    and { exp = _; ty = tybody } = transExp (venv, tenv, body)
    in 
    if tytest = Types.INT then
      { exp = (); ty = tybody }
    else
      raise (Semantic_error "While test must be of integer type")
  )

  | ForExp {var = v; escape = b; lo = lo; hi = hi; body = body; pos = pos} -> (
    let (venv', tenv') = transDec (venv, tenv, A.VarDec { name =  v; escape = b; typ = None; init = lo; pos = pos } ) in
    let { exp = _; ty = tybody } = transExp (venv', tenv', body) in
      { exp = (); ty = tybody }
  )

  | BreakExp p -> { exp = (); ty = Types.NIL }

  | LetExp {decs; body; pos} -> (
  	let (venv', tenv') = List.fold decs ~init:(venv, tenv) ~f:(fun (v, t) d -> transDec (v, t, d)) in
  	  transExp (venv', tenv', body)
  )
  
  | ArrayExp {typ; size; init; pos} -> { exp = (); ty = Types.NIL }

and transTy (tenv, ty) = 
  let open A in
  let open T in
  match ty with 
  | NameTy (symbol, pos) -> 
    (match S.look (tenv, symbol) with
    | Some ty' -> ty'
    | None -> raise (Semantic_error "Unknown type"))
  | RecordTy fields -> let ftypes =  List.map fields ~f:(fun {typ; _} -> 
    match S.look (tenv, typ) with
    | Some ty' -> (typ, ty')
    | None -> raise (Semantic_error "Unknown type")
  ) in RECORD (ftypes, ref ())
  | ArrayTy (symbol, pos) ->  (match S.look (tenv, symbol) with
    | Some ty' -> ARRAY (ty', ref ())
    | None -> raise (Semantic_error "Unknown type"))

and transDec (venv, tenv, dec) = 
  let open A in
  match dec with
  | FunctionDec l -> (venv, tenv)
  | VarDec { name; escape; typ; init; pos } -> 
    let { exp = _; ty = tyinit } = transExp (venv, tenv, init) in
    let open E in
    (match typ with
    | Some (symbol, pos) ->
      (match Symbol.look (tenv, symbol) with
      | Some ty ->
        if ty = tyinit then
          (S.enter (venv, name, Varentry {ty = tyinit}), tenv)
        else
          raise (Semantic_error "Type of initiialyzer does not match type annotation")
      | None -> raise (Semantic_error "unknown type name"))
    | None -> (S.enter (venv, name, Varentry {ty = tyinit}), tenv))
  | TypeDec l ->
    let open T in
    (match l with
      | [ {name; ty; pos} ] -> 
        (venv, S.enter (tenv, name, transTy(tenv, ty)))
      | _ -> 
        let tenv' = List.fold l ~init:tenv ~f:(fun t {name; _} -> 
          S.enter (t, name, NAME(name, ref None))
        ) in
        (List.iter l ~f:(fun {name; ty; pos} -> 
          let typlaceholder = S.look(tenv', name) in
          match typlaceholder with
          | Some NAME(name, tyref) -> 
            tyref := Some (transTy(tenv', ty))
          | _ -> ()
        );
        (venv, tenv')))

and transVar (venv, tenv, var) =
  let open A in
  match var with
  | SimpleVar (symbol, pos) -> 
    let open Env in
    (match Symbol.look (venv, symbol) with
    | Some(entry) -> 
      (match entry with
      | Varentry {ty} ->  { exp = (); ty = actual_ty ty }
      | FunEntry {formals; result} -> raise (Semantic_error "function is not value"))
    | None -> raise (Semantic_error "unknown variable name"))
  | FieldVar (var, sym, pos) -> 
    (let {exp; ty} = transVar (venv, tenv, var) in
      let open A in
      match ty with
      | T.RECORD (fields, _) ->
        (match List.find fields (fun (sym', _) -> sym == sym')  with
        | Some (_, ty) -> { exp = (); ty = actual_ty ty }
        | None -> raise (Semantic_error "unknown field for record")) 
      | _ -> raise (Semantic_error "var isn't a record"))
  | SubscriptVar (var, exp, pos) -> 
    (let {exp; ty} = transVar (venv, tenv, var) in
      match ty with
      | T.ARRAY (ty, _) -> { exp = (); ty = actual_ty ty }
      | _ -> raise (Semantic_error "subscripted is not an array"))

let transProg e0 = transExp (Env.base_venv, Env.base_tenv, e0)
