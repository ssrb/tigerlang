module type T = sig

module Temp : Temp.T

type exp
type level
type access

val outermost: level
val newLevel: parent:level -> name:Temp.label -> formals:bool list -> level
val formals: level -> access list
val allocLocal: level -> bool -> access

val transNil: unit -> exp
val transInt: int -> exp
val transOp: Absyn.oper * exp * exp -> exp
val transVar: access * level -> exp

val toDo: unit -> exp

end

module F = functor(Frame : Frame.T) ->
struct

module Tree = Frame.Tree
module Temp = Tree.Temp

open Core

type exp = Ex of Tree.exp | Nx of Tree.stm | Cx of (Tree.label * Tree.label -> Tree.stm)
type _level = {level: level; frame: Frame.frame}
and level = Outermost | Level of _level * unit ref
type access = level * Frame.access

let outermost = Outermost

let newLevel ~parent ~name ~formals =
Level ({level = parent; frame = Frame.newFrame ~name ~formals:(true::formals)}, ref ())

let formals lvl = 
match lvl with
| Outermost -> assert(false)
| Level ({frame; _}, _) -> frame |> Frame.formals |> List.tl_exn |> List.map ~f:(fun acc -> (lvl, acc))

let allocLocal lvl escape = 
match lvl with
| Outermost -> assert(false)
| Level ({frame; _}, _) -> (lvl, Frame.allocLocal frame escape)

let rec seq stms =
    let module T = Tree in
    match stms with
    | [] -> assert(false)
    | [ stm ] -> stm
    | stm::stms' -> T.SEQ(stm, seq stms')

let unEx exp = 
    let module T = Tree in
    match exp with 
    | Ex ex -> ex
    | Nx nx -> T.ESEQ(nx, T.CONST 0)
    | Cx cx -> 
        let r = Temp.newtemp () in
        let t = Temp.newlabel () in
        let f = Temp.newlabel () in
        T.ESEQ(seq [
            T.MOVE(Tree.TEMP r, T.CONST 1); 
            cx (t, f); 
            T.LABEL f; 
            T.MOVE(Tree.TEMP r, T.CONST 0); 
            T.LABEL t], 
            T.TEMP r)

let unNx exp =
    let module T = Tree in
    match exp with 
    | Ex ex -> T.EXP ex
    | Nx nx -> nx
    | Cx cx -> 
        let l = Temp.newlabel() in
        seq [cx(l, l); T.LABEL l]

let unCx exp = 
    let module T = Tree in
    match exp with 
    | Ex ex ->
    begin
        match ex with
        | T.CONST 0 -> (fun (t, f) -> T.JUMP ((T.NAME f), [f]))
        | T.CONST _ -> (fun (t, f) -> T.JUMP ((T.NAME t), [t]))
        | ex -> (fun (t, f) -> T.CJUMP (T.EQ, (T.CONST 0), ex, f, t))
    end
    | Nx nx -> assert(false)
    | Cx cx -> cx

let transNil i = Ex (Tree.CONST 0)
let transInt i = Ex (Tree.CONST i)

let transOp (op, left, right) =
    let module A = Absyn in
    let module T = Tree in
    let leftEx = unEx left in
    let rightEx = unEx left in
    match op with 
    | A.PlusOp -> Ex (T.BINOP (T.PLUS, leftEx, rightEx))
    | A.MinusOp -> Ex (T.BINOP (T.MINUS, leftEx, rightEx))
    | A.MulOp -> Ex (T.BINOP (T.MUL, leftEx, rightEx))
    | A.DivOp -> Ex (T.BINOP (T.DIV, leftEx, rightEx))
    | A.EqOp -> Ex (T.CONST 0)
    | A.NeqOp -> Ex (T.CONST 0)
    | A.LtOp -> Ex (T.CONST 0)
    | A.LeOp ->Ex (T.CONST 0)
    | A.GtOp -> Ex (T.CONST 0)
    | A.GeOp -> Ex (T.CONST 0)

let transVar (((declvl ,  access) : access), uselvl) = 
    let module T = Tree in

    let level_equal left right =
        match (left, right) with 
        | (Outermost, Outermost) -> true
        | (Outermost, _) | (_, Outermost) -> false
        | (Level (_, left), Level (_, right)) -> left = right
    in

    let rec follow_static_link lvl fp =
        if level_equal declvl lvl then
            fp
        else
            match lvl with
            | Outermost -> assert(false)
            | Level ({level = lvl'; frame}, _) -> 
                let static_link = Frame.formals frame |> List.hd_exn in
                follow_static_link lvl' (Frame.exp (static_link, fp))
    in 
    
    Ex (Frame.exp (access, follow_static_link uselvl (T.TEMP Frame.fp)))
    
let toDo () = Ex (Tree.CONST 0)

end