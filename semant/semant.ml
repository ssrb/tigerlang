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

  let trexp (e) = transExp (venv, tenv, lvl, e, break) in
  
  match exp with
  
  | VarExp v -> transVar (venv, tenv, lvl, v, break)

  | NilExp -> {exp = T.transNil (); ty = Types.NIL}

  | IntExp i -> {exp = T.transInt i; ty = Types.INT}

  | StringExp (s, p) ->  {exp = T.transString s; ty = Types.STRING}
  
  | CallExp c ->
    begin
      match Symbol.look (venv, c.func) with
      | Some(entry) ->
        begin
          match entry with
          | VarEntry _ -> raise (Semantic_error "symbol is not a function")
          | FunEntry fentry ->
            begin

              if (List.length c.args) <> (List.length fentry.formals) then 
                raise (Semantic_error "missing argument in function call");

              let args = List.map c.args ~f:trexp in
              let rty = actual_ty fentry.result in

              args |>  List.zip_exn fentry.formals |> List.iter ~f:(fun(f, a) ->
                if not (type_equal f a.ty) then
                  raise (Semantic_error "wrong type in function call");
              );

              {exp = T.transCall (lvl, fentry.level, fentry.label, (args |> List.map ~f:(fun a -> a.exp)), rty); ty = rty}
              
            end
        end
      | None -> raise (Semantic_error "unknown function name")
    end
  
  | OpExp o ->
    let left = trexp (o.left) in
    let right = trexp (o.right) in
    
    if not (type_equal left.ty Types.INT) || not (type_equal right.ty Types.INT) then
      raise (Semantic_error "integer expected");
    {exp = T.transOp (o.oper, left.exp, right.exp); ty = Types.INT}
  
  | RecordExp r ->
    begin
      match List.find_a_dup r.fields ~compare:(fun (left, _, _) (right,_, _) -> compare left right) with
      | Some dup -> raise (Semantic_error "duplicate record field iniitialization")
      | None -> ();

      match Symbol.look (tenv, r.typ) with
      | Some rty -> 
        begin
          match rty with
          | Types.RECORD (ftypes, _) ->
          begin

            List.iter r.fields ~f:(fun (sym, _, _) ->
            if not (List.exists ftypes ~f:(fun (sym', _) -> sym = sym')) then
              raise (Semantic_error "unknown record field")
            );

            let f (sym, ty) =
              match List.find r.fields ~f:(fun (sym', _, _) -> sym = sym') with
              | Some (_, field, _) ->
                let expty = trexp field in
                if not (type_equal ty expty.ty)  then
                  raise (Semantic_error "wrong type for record field");
                Some expty.exp  
              | None -> None
            in
 
            {exp = T.transRecord (List.map ftypes ~f:f); ty = rty}

          end
          | _ -> raise (Semantic_error "not a record type")
        end
      | None -> raise (Semantic_error "unknown record type")
    end

  | SeqExp l ->
    let ts = List.fold l ~init:[] ~f:(fun ts (t, _) -> (trexp t)::ts) in
    {exp = T.transSeq(ts |> List.rev_map ~f:(fun x -> x.exp)); ty = (ts |> List.hd_exn).ty} 

  | AssignExp a ->
    let var = transVar (venv, tenv, lvl, a.var, break) in
    let exp = trexp a.exp in
    if not (type_equal var.ty exp.ty) then 
      raise (Semantic_error "Incompatible type in assignment");
    {exp = T.transAssign (var.exp, exp.exp); ty = Types.UNIT}
    
  | IfExp i ->
    let test = trexp i.test in
    let then' = trexp i.then' in
    begin
      if not (type_equal test.ty Types.INT) then
        raise (Semantic_error "If test must be of integer type");
      match i.else' with
      | Some else' -> 
        let else' = trexp else' in
        if not (type_equal then'.ty else'.ty) then
          raise (Semantic_error "If-then-else type is inconsistent");
        {exp = T.transIf (test.exp, then'.exp, Some else'.exp); ty = then'.ty}
      | None -> {exp = T.transIf (test.exp, then'.exp, None); ty = Types.UNIT}
    end

  | WhileExp w ->
    let test = trexp w.test in
    let finish = Temp.newlabel() in
    let body = transExp (venv, tenv, lvl, w.body, Some finish) in
    if not (type_equal test.ty Types.INT) then
      raise (Semantic_error "While test must be of integer type");
    {exp = T.transWhile (test.exp, body.exp, finish); ty = body.ty}
      
  | ForExp f ->
    begin
      let (venv', tenv', inits) = transDec (venv, tenv, lvl, VarDec {name =  f.var; escape = f.escape; typ = None; init = f.lo; pos = f.pos}, break) in
      match S.look (venv, f.var) with
      | Some (Env.VarEntry var) -> 
        if not (type_equal var.ty INT) then 
          raise (Semantic_error "For loop lower bound must have int type");
        let hi = transExp (venv, tenv, lvl, f.hi, break) in
        if not (type_equal hi.ty INT) then 
          raise (Semantic_error "For loop upper bound must have int type");
        let finish = Temp.newlabel() in
        let body = transExp (venv', tenv', lvl, f.body, Some finish) in
        {exp = T.transFor (var.access, List.hd_exn inits, hi.exp, body.exp, finish); ty = body.ty}
      | _ -> assert(false)
    end

  | BreakExp b ->
    begin
      match break with
      | Some label -> {exp = T.transBreak label; ty = Types.UNIT}
      | None -> raise (Semantic_error "No loop to break from")
    end

  | LetExp l ->
    let f (v, t, is) d = 
      let (v, t, i) = transDec (v, t, lvl, d, break) in
      (v, t, i::is)
    in
  	let (venv', tenv', inits) = List.fold l.decs ~init:(venv, tenv, []) ~f:f in
    let inits = inits |> List.rev |> List.concat  in
  	let body = transExp (venv', tenv', lvl, l.body, break) in
    {exp = T.transLet (inits, body.exp); ty = body.ty}
  
  | ArrayExp a ->
    let size = trexp a.size in
    if not (type_equal size.ty Types.INT) then
       raise (Semantic_error "Array size expression must have type int");
    let init = trexp a.init in
    match S.look(tenv, a.typ) with
    | Some ty ->
      begin
        match ty with
        | ARRAY (ty, unique) -> 
          if not (type_equal ty init.ty) then
            raise (Semantic_error "Incompoatible initializer type");
          {exp = T.transArray (size.exp, init.exp); ty}  
        | _ -> raise (Semantic_error "Not an array type")
      end
    | None -> raise (Semantic_error "Unknown array type")
     
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
      
      let {exp = _; ty = tybody} = transExp(venv'' , tenv, lvl', body, None) in
      match tyresopt with
      | Some tyresult ->
        if not (type_equal tybody tyresult) then
          raise (Semantic_error "Wrong return type")
      | None -> ()
    in
    begin
      fdecs |> List.rev |> List.iter ~f:trans_body;
      (venv', tenv, [])
    end
  
  | VarDec {name; escape; typ; init; pos} -> 
    
    let init = transExp (venv, tenv, lvl, init, break) in
    begin
      match typ with
      | Some (symbol, pos) ->
        begin
          match Symbol.look (tenv, symbol) with
          | Some ty ->
            if not (type_equal ty init.ty) then
              raise (Semantic_error "Type of initiialyzer does not match type annotation")
          | None -> raise (Semantic_error "unknown type name")
        end
      | None -> 
        if init.ty = NIL then
          raise (Semantic_error "A variable declaration initialized with nil must beconstrained to be a structure")
    end;

    (S.enter (venv, name, VarEntry {access = T.allocLocal lvl !escape; ty = init.ty}), tenv, [ init.exp ])

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

    (venv, tenv', [])

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
    let var = transVar (venv, tenv, lvl, var, break) in
    begin
      match var.ty with
      | RECORD (fields, _) ->
        begin
          match List.findi fields (fun _ (sym', _) -> sym = sym')  with
          | Some (i, (_, ty)) -> { exp = T.transField (var.exp, i); ty = actual_ty ty }
          | None -> raise (Semantic_error "unknown field for record")
        end
      | _ -> raise (Semantic_error "var isn't a record")
    end

  | SubscriptVar (var, sub, pos) -> 
    let var = transVar (venv, tenv, lvl, var, break) in
    begin
      match var.ty with
      | ARRAY (ty, _) -> 
        let sub = transExp (venv, tenv, lvl, sub, break) in
        if not (type_equal ty INT) then 
          raise (Semantic_error "subscript must have int type");
        { exp = T.transSubscript (var.exp, sub.exp); ty = actual_ty ty }
      | _ -> raise (Semantic_error "subscripted is not an array")
    end

let transProg e0 = 
  FindEscape.findEscape e0;
  transExp (Env.base_venv, Env.base_tenv, T.outermost, e0, None)
end