type depth = int
type escEnv = (depth * bool ref) Symbol.table

module S = Symbol
open Core
open Absyn

let rec traverseExp (env, d, exp) =
    match exp with
    | VarExp var -> traverseVar (env, d, var)
    | NilExp -> env
    | IntExp _ -> env
    | StringExp _ -> env
    | CallExp {args; _} -> List.fold args ~init:env ~f:(fun env' arg -> traverseExp (env', d, arg))
    | OpExp {left; right; _} -> let env = traverseExp (env, d, left) in traverseExp (env, d, right)
    | RecordExp {fields; _} -> List.fold fields ~init:env ~f:(fun env (_, exp, _) -> traverseExp (env, d, exp))
    | SeqExp exps -> List.fold exps ~init:env ~f:(fun env (exp, _) -> traverseExp (env, d, exp))
    | AssignExp {var; exp; _} -> let env = traverseExp (env, d, exp) in traverseVar (env, d, var)
    | IfExp  {test; then'; else'; _} -> 
        let env = traverseExp (env, d, test) in 
        let env = traverseExp (env, d, then') in
        begin
            match else' with
            | Some else' -> traverseExp (env, d, else')
            | None -> env
        end
    | WhileExp  {test; body; _} -> let env = traverseExp (env, d, test) in traverseExp (env, d, body)
    | ForExp  {var; escape; lo; hi; body; pos} -> 
        let env' = traverseDec (env, d, VarDec {name = var; escape; typ = None; init = lo; pos}) in
        traverseExp (env, d, hi) |> ignore;
        traverseExp (env', d, body)
    | BreakExp _ -> env
    | LetExp {decs; body} -> let env = List.fold decs ~init:env ~f:(fun env dec -> traverseDec (env, d, dec)) in traverseExp (env, d, body)
    | ArrayExp {size; init; _} -> let env = traverseExp (env, d, size) in traverseExp (env, d, init)

and traverseDec (env, d, dec) =  
    match dec with
    | FunctionDec funcs ->
        let f {params; body; _} =
            let d = succ d in
            let env = List.fold params ~init:env ~f:(fun env {name; escape; _} -> S.enter (env, name, (d, escape))) 
            in traverseExp (env, d, body) |> ignore
        in List.iter funcs ~f:f; env 
    | VarDec {name; escape; init} -> escape := false; S.enter (env, name, (d, escape))
    | TypeDec _ -> env

and traverseVar (env, d, var) =
    match var with
    | SimpleVar (symbol, _) ->
    begin
        match S.look (env, symbol) with
        | Some (depth, escape) -> if d > depth then escape := true; env
        | None -> env
    end
    | FieldVar _ -> env
    | SubscriptVar (var , exp, _) -> let env = traverseVar (env, d, var) in traverseExp (env, d, exp)

let findEscape prog = ignore (traverseExp (S.empty, 0, prog))
