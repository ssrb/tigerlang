open Core.Std

module A = Absyn
module S = Symbol
module E = Env
module T = Types

exception Semantic_error of string

type venv = Env.enventry Symbol.table
type tenv = Types.ty Symbol.table

type expty = {exp: Translate.exp; ty: Types.ty}

let type_equal tyleft tyright =
  match (tyleft,tyright) with
  | (T.RECORD _, T.NIL) | (T.NIL, T.RECORD _) -> true
  | (T.NIL, _) | (_, T.NIL)-> false
  | _ -> tyleft = tyright


let rec actual_ty ty =
  match ty with
  | T.NAME (sym, tyref) ->
    (match !tyref with
    | Some ty' -> actual_ty ty'
    | None -> raise (Semantic_error "Incomplete type"))
  | _ -> ty

let rec transExp (venv, tenv, exp, break) = 
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
      | Env.VarEntry _ -> raise (Semantic_error "symbol is not a function")
      | Env.FunEntry {formals; result} -> (
        if (List.length args) <>  (List.length formals) then 
          raise (Semantic_error "missing argument in function call");
        List.zip_exn args formals |> List.iter ~f:(fun (a, ty) -> 
          let { exp = _; ty = ty' } = transExp (venv, tenv, a, break) in 
          if not (type_equal ty ty') then
            (raise (Semantic_error "wrong type in function call"))
        );
        {exp = (); ty = actual_ty result }
      ))
    | None -> raise (Semantic_error "unknown function name"))
  
  | OpExp {left; oper; right; pos} ->
    let {exp = _; ty = tyleft} = transExp (venv, tenv, left, break)
    and {exp = _; ty = tyright} = transExp (venv, tenv, right, break)
    in
    if (type_equal tyleft Types.INT) && (type_equal tyright Types.INT) then
        { exp = (); ty = Types.INT }
    else
        raise (Semantic_error "integer expected")
  
  | RecordExp {fields; typ; pos} ->
    (match Symbol.look (tenv, typ) with
    | Some rty -> (match rty with
      | Types.RECORD (ftypes, _) -> (
          List.iter fields ~f:(fun (sym, exp, pos) -> 
            let { exp = _; ty } = transExp (venv, tenv, exp, break) in 
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
      List.map l (fun e -> transExp (venv, tenv, fst e, break)) |> List.last_exn
  )

  | AssignExp {var; exp; pos} -> (
    let { exp = _; ty = tyleft} = transVar (venv, tenv, var)
    and { exp = _; ty = tyright} = transExp (venv, tenv, exp, break)
    in
      if type_equal tyleft tyright then 
        { exp = (); ty = tyleft }
      else
        raise (Semantic_error "Incompatible type in assignment")
  )

  | IfExp {test; then'; else'; pos} -> (
    let { exp = _; ty = tytest } = transExp (venv, tenv, test, break)
    and { exp = _; ty = tythen } = transExp (venv, tenv, then', break)
    in
      if type_equal tytest Types.INT then
        (match else' with
        | None -> { exp = (); ty = Types.NIL }
        | Some e -> 
          let { exp = _; ty = tyelse } = transExp (venv, tenv, e, break) in
          if type_equal tythen tyelse then
            { exp = (); ty = tythen }
          else
            raise (Semantic_error "If-then-else type is inconsistent"))
      else
        raise (Semantic_error "If test must be of integer type")
    )

  | WhileExp {test; body; pos} -> (
    let { exp = _; ty = tytest } = transExp (venv, tenv, test, break)
    and { exp = _; ty = tybody } = transExp (venv, tenv, body, true)
    in 
    if type_equal tytest Types.INT then
      { exp = (); ty = tybody }
    else
      raise (Semantic_error "While test must be of integer type")
  )

  | ForExp {var = v; escape = b; lo = lo; hi = hi; body = body; pos = pos} -> (
    let (venv', tenv') = transDec (venv, tenv, A.VarDec { name =  v; escape = b; typ = None; init = lo; pos = pos }, break) in
    let { exp = _; ty = tybody } = transExp (venv', tenv', body, true) in
      { exp = (); ty = tybody }
  )

  | BreakExp p -> 
    if break = true then
    { exp = (); ty = Types.NIL }
    else
      raise (Semantic_error "No loop to break from")

  | LetExp {decs; body; pos} -> (
  	let (venv', tenv') = List.fold decs ~init:(venv, tenv) ~f:(fun (v, t) d -> transDec (v, t, d, break)) in
  	  transExp (venv', tenv', body, break)
  )
  
  | ArrayExp {typ; size; init; pos} ->
    let {exp = _; ty = tysize} = transExp(venv, tenv, size, break) in
    if type_equal tysize T.INT then
      let {exp = _; ty = tyinit} = transExp(venv, tenv, init, break) in
      match S.look(tenv, typ) with
      | Some tyarray ->
        (match tyarray with
        | T.ARRAY (tyelem, unique) -> 
          if type_equal tyelem tyinit then
            { exp = (); ty = tyarray }
          else
            raise (Semantic_error "Incompoatible initializer type")
        | _ ->
            raise (Semantic_error "Not an array type")
        )
      | None ->
        raise (Semantic_error "Unknown array type")
    else
      raise (Semantic_error "Array size expression must have type int")

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

and transDec (venv, tenv, dec, break) = 
  let open A in
  match dec with
  
  | FunctionDec l ->
    let open T in
    let open E in
    let venv' = List.fold l ~init:venv ~f:(fun v { name; params; result; _ } ->
      let params' = List.map params ~f:(fun { typ; _ } ->
        match S.look (tenv, typ) with
        | Some ty' -> ty'
        | None -> raise (Semantic_error "Unknown type")
      ) in
      let result' =  
        match result with
        | Some (res, pos) -> 
          (match S.look (tenv, res) with
          | Some ty' -> ty'
          | None -> raise (Semantic_error "Unknown type"))
        | None -> UNIT
      in S.enter (v, name, FunEntry {formals = params'; result = result'})
    )
    in
    (List.iter l ~f:(fun { name; params; result; body; pos} -> 
      let venv'' = List.fold params ~init:venv' ~f:(fun v { name; typ; _ } ->
        match S.look (tenv, typ) with
        | Some ty' -> S.enter (v, name, VarEntry {ty = ty'})
        | None -> raise (Semantic_error "Unknown type")
      )
      in
      match result with
      | Some (res, pos) -> 
        (match S.look (tenv, res) with
        | Some ty' ->
          let {exp = _; ty = ty''} = transExp(venv'' , tenv, body, false) in
          if not (type_equal ty' ty'') then
            raise (Semantic_error "Wrong return type")
        | None -> raise (Semantic_error "Unknown type"))
      | None -> ()
    );
    (venv', tenv))
  
  | VarDec { name; escape; typ; init; pos } -> 
    let { exp = _; ty = tyinit } = transExp (venv, tenv, init, break) in
    let open E in
    (match typ with
    | Some (symbol, pos) ->
      (match Symbol.look (tenv, symbol) with
      | Some ty ->
        if type_equal ty tyinit then
          (S.enter (venv, name, VarEntry {ty = tyinit}), tenv)
        else
          raise (Semantic_error "Type of initiialyzer does not match type annotation")
      | None -> raise (Semantic_error "unknown type name"))
    | None -> 
      if tyinit = T.NIL then
        raise (Semantic_error "A variable declaration initialized with nil must beconstrained to be a structure")
      else
        (S.enter (venv, name, VarEntry {ty = tyinit}), tenv))
  
  | TypeDec l ->
    let open T in
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
    (venv, tenv'))

and transVar (venv, tenv, var) =
  let open A in
  match var with
  | SimpleVar (symbol, pos) -> 
    let open Env in
    (match Symbol.look (venv, symbol) with
    | Some(entry) -> 
      (match entry with
      | VarEntry {ty} ->  { exp = (); ty = actual_ty ty }
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

let transProg e0 = transExp (Env.base_venv, Env.base_tenv, e0, false)
