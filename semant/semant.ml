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

let type_equal left right =
  match (left, right) with
  | (NIL, RECORD _) | (RECORD _, NIL) -> true
  | (NIL, _) | (_, NIL)-> false
  | (RECORD (_, left), RECORD (_, right))
  | (ARRAY (_, left), ARRAY (_, right))-> phys_equal left right
  | _ -> left = right

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

              {exp = T.transCall (fentry.level, lvl, fentry.label, (args |> List.map ~f:(fun a -> a.exp)), rty); ty = rty}
              
            end
        end
      | None -> raise (Semantic_error "unknown function name")
    end
  
  | OpExp o ->
  begin
    let left = trexp (o.left) in
    let right = trexp (o.right) in
    
    (match o.oper with
    | PlusOp | MinusOp | MulOp | DivOp ->
      if not (type_equal left.ty Types.INT) || not (type_equal right.ty Types.INT) then
        raise (Semantic_error "integer expected");
    | EqOp | NeqOp ->
      if not (type_equal left.ty right.ty) then
        raise (Semantic_error "different types in equality test");
    | LtOp | LeOp | GtOp | GeOp ->
     if not (((type_equal left.ty Types.INT) && (type_equal right.ty Types.INT))
     || ((type_equal left.ty Types.STRING) && (type_equal right.ty Types.STRING))) then
        raise (Semantic_error "integer or string expected"));

    let ty = (if type_equal left.ty Types.NIL then right.ty else left.ty) in 

    {exp = T.transOp (o.oper, left.exp, right.exp, ty); ty = Types.INT}

  end

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
              raise (Semantic_error ("unknown record field " ^ Symbol.name sym))
            );

            let f (sym, ty) =
              match List.find r.fields ~f:(fun (sym', _, _) -> sym = sym') with
              | Some (_, field, _) ->
                let ty = actual_ty ty in
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
  begin
    match l with
    | [] -> {exp = T.transNop (); ty = Types.UNIT}
    | _ -> 
      let ts = List.fold l ~init:[] ~f:(fun ts (t, _) -> (trexp t)::ts) in
      {exp = T.transSeq(ts |> List.rev_map ~f:(fun x -> x.exp)); ty = (ts |> List.hd_exn).ty} 
  end

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
      | None -> 
        if not (type_equal then'.ty Types.UNIT) then
          raise (Semantic_error "If-then statement must have unit type");
        {exp = T.transIf (test.exp, then'.exp, None); ty = Types.UNIT}
    end

  | WhileExp w ->
    let test = trexp w.test in
    let finish = Temp.newlabel() in
    let body = transExp (venv, tenv, lvl, w.body, Some finish) in
    if not (type_equal test.ty Types.INT) then
      raise (Semantic_error "While test must have integer type");
    if not (type_equal body.ty Types.UNIT) then
      raise (Semantic_error "While body must have unit type");
    {exp = T.transWhile (test.exp, body.exp, finish); ty = body.ty}
      
  | ForExp f ->
    begin
      let (venv', tenv', inits) = transDec (venv, tenv, lvl, VarDec {name =  f.var; escape = f.escape; typ = None; init = f.lo; pos = f.pos}, break) in
      match S.look (venv', f.var) with
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
        | ARRAY (ety, _) -> 
          if not (type_equal ety init.ty) then
            raise (Semantic_error "Incompatible initializer type");
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
  | RecordTy fields -> let ftypes =  List.map fields ~f:(fun f -> 
    match S.look (tenv, f.typ) with
    | Some ty -> (f.name, ty)
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

  | FunctionDec fs ->
  begin
     match List.find_a_dup fs ~compare:(fun (left : fundec) (right : fundec) -> compare left.name right.name) with
    | Some dup -> raise (Semantic_error "duplicate function name within the same batch")
    | None -> ();

    let forward_declare (fdecs, v) f =
      
      let typarams = List.map f.params ~f:(fun p ->
        match S.look (tenv, p.typ) with
        | Some ty -> (p.name, ty, !(p.escape))
        | None -> raise (Semantic_error "Unknown type")
      )
      in
      
      let lab = Temp.newlabel () in
      
      let tyres =
        match f.result with
        | Some (res, pos) ->
        begin
          match S.look (tenv, res) with
          | Some ty -> ty
          | None -> raise (Semantic_error "Unknown type")
        end
        | None -> UNIT
      in

      let v' = S.enter (v, f.name, FunEntry {
        level = lvl;
        label = lab;
        formals = typarams |> List.map ~f:(fun (n, t, e) -> t); 
        result = tyres
      })
      in

      ((lab, typarams, tyres, f.body)::fdecs, v')

    in
    let (fdecs, venv') = List.fold fs ~init:([], venv) ~f:forward_declare in
    let trans_body (lab, typarams, tyres, body) =

      let lvl' = T.newLevel ~parent:lvl ~name:lab ~formals:(List.map typarams ~f:(fun (_, _, e) -> e)) in

      let venv'' = List.fold2_exn ~init:venv' ~f:(fun v (n, ty, _) access ->
        S.enter (v, n, VarEntry {access; ty})
      ) typarams (T.formals lvl')
      in
      
      let body = transExp(venv'' , tenv, lvl', body, None) in
      if not (type_equal body.ty tyres) then
          raise (Semantic_error "Wrong return type");
      
      T.procEntryExit ~level:lvl' ~body:body.exp
    in

    fdecs |> List.rev |> List.iter ~f:trans_body;
    (venv', tenv, [])

  end

  | VarDec v -> 
    
    let init = transExp (venv, tenv, lvl, v.init, break) in
    let ty = 
      match v.typ with
      | Some (symbol, pos) ->
        begin
          match Symbol.look (tenv, symbol) with
          | Some ty ->
            if not (type_equal ty init.ty) then
              raise (Semantic_error "Type of initialyzer does not match type annotation");
            ty
          | None -> raise (Semantic_error "unknown type name")
        end
      | None -> 
        if init.ty = NIL then
          raise (Semantic_error "A variable declaration initialized with nil must be constrained to be a structure");
        init.ty
    in

    (S.enter (venv, v.name, VarEntry {access = T.allocLocal lvl !(v.escape); ty}), tenv, [ init.exp ])

  | TypeDec ts ->
  begin 
    match List.find_a_dup ts ~compare:(fun (left : typedec) (right : typedec) -> compare left.name right.name) with
    | Some dup -> raise (Semantic_error "duplicate type name within the same batch")
    | None -> ();

    let (fdecs, tenv) = List.fold ts ~init:([], tenv) ~f:(fun (fdecs, te) t ->
      let fdec = NAME(t.name, ref None) in
      ((fdec, t)::fdecs, S.enter (te, t.name, fdec))
    ) 
    in

    let tenv = fdecs |> List.rev |> List.fold ~init:tenv ~f:(fun te (fdec, (t : typedec)) -> 
      match fdec with
      | NAME (name, tyref) -> 
        let ty = transTy(te, t.ty) in
         tyref := Some ty;
         S.enter (te, t.name, ty)
      | _ -> assert(false)
    ) 
    in

    List.iter ts ~f:(fun t -> checkForCyclicType(tenv, t.name));
    (venv, tenv, [])
  end

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
        if not (type_equal sub.ty INT) then 
          raise (Semantic_error "subscript must have int type");
        { exp = T.transSubscript (var.exp, sub.exp); ty = actual_ty ty }
      | _ -> raise (Semantic_error "subscripted is not an array")
    end

let transProg e0 = 
  FindEscape.findEscape e0;
  let lab = Temp.newlabel () in
  let level = T.newLevel ~parent:T.outermost ~name:lab ~formals:[] in
  transExp (Env.base_venv, Env.base_tenv, level, e0, None)

let transProg2 e0 =
  FindEscape.findEscape e0;
  let lab = Temp.newlabel () in
  let level =  T.newLevel ~parent:T.outermost ~name:lab ~formals:[] in
  let expty = transExp (Env.base_venv, Env.base_tenv, level, e0, None) in
  T.procEntryExit ~body:expty.exp ~level;
  T.getResult ()

end