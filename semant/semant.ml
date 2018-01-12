module F = functor (Translate: Translate.T) -> struct  

module S = Symbol
module T = Translate
module Env = Env.F(Translate)

open Core
open Absyn
open S
open T
open Env
open Types

type venv = Env.enventry Symbol.table
type tenv = Types.ty Symbol.table
type expty = {exp: Translate.exp; ty: Types.ty}

exception Semantic_error of string

let type_equal tyleft tyright =
  match (tyleft, tyright) with
  | (NIL, RECORD _) | (RECORD _, NIL) -> true
  | (NIL, _) | (_, NIL)-> false
  | _ -> tyleft = tyright

let rec actual_ty ty =
  match ty with
  | NAME (sym, actual) ->
    begin
      match !actual with
      | Some ty' -> actual_ty ty'
      | None -> assert(false)
    end
  | _ -> ty

let rec transExp (venv, tenv, lvl, exp, break) =

  let trexp (e, break) = transExp (venv, tenv, lvl, e, break) in
  
  match exp with
  
  | VarExp v -> transVar (venv, tenv, lvl, v, break)

  | NilExp -> {exp = T.transNil (); ty = Types.NIL}

  | IntExp i -> {exp = T.transInt i; ty = Types.INT}

  | StringExp (s, p) ->  {exp = T.toDo (); ty = Types.STRING}
  
  | CallExp {func; args; pos} ->
    begin
      match Symbol.look (venv, func) with
      | Some(entry) ->
        begin
          match entry with
          | VarEntry _ -> raise (Semantic_error "symbol is not a function")
          | FunEntry {formals; result} ->
            begin
              if (List.length args) <>  (List.length formals) then 
                raise (Semantic_error "missing argument in function call");

              let exp = List.map args ~f:(fun a -> trexp (a, break))
              |> List.zip_exn formals 
              |> List.fold ~init:(T.toDo ()) ~f:(fun exp (f, {ty; _}) ->
                if not (type_equal f ty) then
                  raise (Semantic_error "wrong type in function call");
                exp
              )
              in {exp = exp; ty = actual_ty result}
            end
        end
      | None -> raise (Semantic_error "unknown function name")
    end
  
  | OpExp {left; oper; right; pos} ->

    let {exp = expLeft; ty = tyleft} = trexp (left, break) in
    let {exp = expRight; ty = tyright} = trexp (right, break) in
    if (type_equal tyleft Types.INT) && (type_equal tyright Types.INT) then
     {exp = T.transOp (oper, expLeft, expRight); ty = Types.INT}
    else
     raise (Semantic_error "integer expected")
  
  | RecordExp {fields; typ; pos} ->
    begin
      match Symbol.look (tenv, typ) with
      | Some rty -> 
        begin
          match rty with
          | Types.RECORD (ftypes, _) ->
            let exp = List.fold fields ~init:(T.toDo ()) ~f:(fun e (sym, exp, pos) ->
              let {exp = _; ty} = trexp (exp, break) in 
              if not (List.exists ftypes (fun ft -> ft = (sym, ty))) then
                raise (Semantic_error "not a record type");
              e
            ) 
            in {exp = exp; ty = rty}
          | _ -> raise (Semantic_error "not a record type")
        end
      | None -> raise (Semantic_error "unknown record type")
    end

  | SeqExp l ->
    List.fold l ~init:{exp = T.toDo (); ty = Types.UNIT} ~f:(fun _ exp -> 
      trexp (fst exp, break)
    )

  | AssignExp {var; exp; pos} ->
    let {exp = _; ty = tyleft} = transVar (venv, tenv, lvl, var, break) in
    let {exp = _; ty = tyright} = trexp (exp, break) in
    if type_equal tyleft tyright then 
      {exp = T.toDo (); ty = tyleft}
    else
      raise (Semantic_error "Incompatible type in assignment")

  | IfExp {test; then'; else'; pos} ->
    let {exp = _; ty = tytest} = trexp (test, break) in
    let {exp = _; ty = tythen} = trexp (then', break) in
    if type_equal tytest Types.INT then
      match else' with
      | None -> {exp = T.toDo (); ty = Types.NIL}
      | Some e -> 
        let {exp = _; ty = tyelse} = trexp (e, break) in
        if type_equal tythen tyelse then
          {exp = T.toDo (); ty = tythen}
        else
          raise (Semantic_error "If-then-else type is inconsistent")
    else
      raise (Semantic_error "If test must be of integer type")

  | WhileExp {test; body; pos} ->
    let {exp = _; ty = tytest} = trexp (test, break) in
    let {exp = _; ty = tybody} = trexp (body, true) in
    if type_equal tytest Types.INT then
      {exp = T.toDo (); ty = tybody}
    else
      raise (Semantic_error "While test must be of integer type")

  | ForExp {var; escape; lo; hi; body = body; pos} ->
    let (venv', tenv') = transDec (venv, tenv, lvl, VarDec {name =  var; escape; typ = None; init = lo; pos}, break) in
    (* TODO: assert INT*)
    let {exp; ty = tyhi} = transExp (venv, tenv, lvl, hi, break) in
    if not (type_equal tyhi INT) then raise (Semantic_error "For loop upper bound must have int type");
    let {exp; ty = tybody} = transExp (venv', tenv', lvl, body, true) in
    {exp = T.toDo (); ty = tybody}

  | BreakExp p ->
    if break then
      {exp = T.toDo (); ty = Types.NIL}
    else
      raise (Semantic_error "No loop to break from")

  | LetExp {decs; body; pos} ->
  	let (venv', tenv') = List.fold decs ~init:(venv, tenv) ~f:(fun (v, t) d -> transDec (v, t, lvl, d, break)) in
  	transExp (venv', tenv', lvl, body, break)
  
  | ArrayExp {typ; size; init; pos} ->
    let {exp = _; ty = tysize} = trexp(size, break) in
    if type_equal tysize INT then
      let {exp = _; ty = tyinit} = trexp(init, break) in
      match S.look(tenv, typ) with
      | Some tyarray ->
        begin
          match tyarray with
          | ARRAY (tyelem, unique) -> 
            if type_equal tyelem tyinit then
              {exp = T.toDo (); ty = tyarray}
            else
              raise (Semantic_error "Incompoatible initializer type")
          | _ ->
            raise (Semantic_error "Not an array type")
        end
      | None ->
        raise (Semantic_error "Unknown array type")
    else
      raise (Semantic_error "Array size expression must have type int")

and transTy (tenv, ty) = 
  match ty with 
  | NameTy (symbol, pos) -> 
    begin
      match S.look (tenv, symbol) with
      | Some ty' -> ty'
      | None -> raise (Semantic_error "Unknown type")
    end
  | RecordTy fields -> let ftypes =  List.map fields ~f:(fun {typ; _} -> 
    match S.look (tenv, typ) with
    | Some ty' -> (typ, ty')
    | None -> raise (Semantic_error "Unknown type")
  ) in RECORD (ftypes, ref ())
  | ArrayTy (symbol, pos) ->  
    begin
      match S.look (tenv, symbol) with
      | Some ty' -> ARRAY (ty', ref ())
      | None -> raise (Semantic_error "Unknown type")
    end

and checkForCyclicType (tenv, symbol) =
  let open Option.Monad_infix in
  
  let step symopt =
    symopt >>= (fun sym ->
    S.look (tenv, sym) >>= (fun ty ->
    match ty with
    | NAME (_, tyref) -> !tyref >>= (function
        | NAME (sym', _) -> Some sym'
        | _ -> None
      )
    | _ -> None))
  in 
  
  let rec followCycle sym1 sym2 = 
    match (sym1, sym2) with 
    | (None, _) | (_, None) -> ()
    | (Some sym1, Some sym2) when sym1 = sym2 ->
      raise (Semantic_error "cycle detected") 
    | _ -> followCycle (sym1 |> step) (sym2 |> step |> step)
  in
  
  let sym = Some symbol in

  followCycle sym (sym |> step |>step)

and transDec (venv, tenv, lvl, dec, break) = 
  match dec with

  | FunctionDec l ->

    let forward_declare (fdecs, v) {name; params; result; body} =
      
      let typarams = List.map params ~f:(fun {typ = pname; _} ->
        match S.look (tenv, pname) with
        | Some ty -> (pname, ty)
        | None -> raise (Semantic_error "Unknown type")
      )
      in
      
      let lab = Temp.newlabel () in
      let lvl' = T.newLevel ~parent:lvl ~name:lab ~formals:(List.map params ~f:(fun t -> true)) in

      let tyresopt =
        match result with
        | Some (res, pos) ->
        begin
          match S.look (tenv, res) with
          | Some _ as tyopt -> tyopt
          | None -> raise (Semantic_error "Unknown type")
        end
        | None -> None
      in

      let v' = S.enter (v, name, FunEntry {
        level = lvl';
        label = lab;
        formals = typarams |> List.map ~f:(fun (n,t) -> t); 
        result = match tyresopt with Some ty -> ty | None -> UNIT
      })
      in

      ((name, lvl', typarams, tyresopt, body)::fdecs, v') 
    in
    let (fdecs, venv') = List.fold l ~init:([], venv) ~f:forward_declare in
    let trans_body (name, lvl', typarams, tyresopt, body) =

      let venv'' = List.fold typarams ~init:venv' ~f:(fun v (n, t) ->
        S.enter (v, n, VarEntry {access = T.allocLocal lvl' true; ty = t})
      )
      in
      
      let {exp = _; ty = tybody} = transExp(venv'' , tenv, lvl', body, false) in
      match tyresopt with
      | Some tyresult ->
        if not (type_equal tybody tyresult) then
          raise (Semantic_error "Wrong return type")
      | None -> ()
    in
    begin
      fdecs |> List.rev |> List.iter ~f:trans_body;
      (venv', tenv)
    end
  
  | VarDec {name; escape; typ; init; pos} -> 
    
    let {exp = _; ty = tyinit} = transExp (venv, tenv, lvl, init, break) in
    begin
      match typ with
      | Some (symbol, pos) ->
        begin
          match Symbol.look (tenv, symbol) with
          | Some ty ->
            if not (type_equal ty tyinit) then
              raise (Semantic_error "Type of initiialyzer does not match type annotation")
          | None -> raise (Semantic_error "unknown type name")
        end
      | None -> 
        if tyinit = NIL then
          raise (Semantic_error "A variable declaration initialized with nil must beconstrained to be a structure")
    end;

    (S.enter (venv, name, VarEntry {access = T.allocLocal lvl true; ty = tyinit}), tenv)

  | TypeDec l ->
    
    let (fdecs, tenv') = List.fold l ~init:([], tenv) ~f:(fun (fdecs, te) {name; ty} ->
      let fdec = NAME(name, ref None) in
      ((fdec, ty)::fdecs, S.enter (te, name, fdec))
    ) 
    in

    fdecs |> List.rev |> List.iter ~f:(fun (fdec, ty) -> 
      match fdec with
      | NAME (name, tyref) -> 
        tyref := Some (transTy(tenv', ty))
      | _ -> assert(false)
    );

    List.iter l ~f:(fun {name; ty; pos} -> checkForCyclicType(tenv', name));

    (venv, tenv')

and transVar (venv, tenv, lvl, var, break) =

  match var with

  | SimpleVar (symbol, pos) -> 
    begin
      match S.look (venv, symbol) with
      | Some(entry) -> 
        begin
        match entry with
          | VarEntry {access; ty} ->  {exp = T.transVar (access , lvl); ty = actual_ty ty}
          | FunEntry {formals; result} -> raise (Semantic_error "function is not value")
        end
      | None -> raise (Semantic_error "unknown variable name")
    end

  | FieldVar (var, sym, pos) ->
    let {exp; ty} = transVar (venv, tenv, lvl, var, break) in
    begin
      match ty with
      | RECORD (fields, _) ->
        begin
          match List.find fields (fun (sym', _) -> sym = sym')  with
          | Some (_, ty) -> { exp = T.toDo (); ty = actual_ty ty }
          | None -> raise (Semantic_error "unknown field for record")
        end
      | _ -> raise (Semantic_error "var isn't a record")
    end

  | SubscriptVar (var, sub, pos) -> 
    let {exp; ty} = transVar (venv, tenv, lvl, var, break) in
    begin
      match ty with
      | ARRAY (ty, _) -> 
      let {exp; ty} = transExp (venv, tenv, lvl, sub, break) in
      if not (type_equal ty INT) then raise (Semantic_error "subscript must have int type");
      { exp = T.toDo (); ty = actual_ty ty }
      | _ -> raise (Semantic_error "subscripted is not an array")
    end

let transProg e0 = 
  FindEscape.findEscape e0;
  transExp (Env.base_venv, Env.base_tenv, T.outermost, e0, false)
end