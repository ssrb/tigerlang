module type T = sig

module Frame: Frame.T
module Temp : Temp.T

type exp
type level
type access

val outermost: level
val newLevel: parent:level -> name:Temp.label -> formals:bool list -> level
val formals: level -> access list
val allocLocal: level -> bool -> access
val getResult: unit -> Frame.frag list

val transNil: unit -> exp
val transInt: int -> exp
val transOp: Absyn.oper * exp * exp -> exp
val transSeq: exp list -> exp
val transString: string -> exp
val transCall: level * level * Temp.label * exp list * Types.ty -> exp
val transRecord: exp option list -> exp
val transVar: access * level -> exp

val toDo: unit -> exp

end

module F = functor(Frame : Frame.T) ->
struct

module Frame = Frame
module Tree = Frame.Tree
module Temp = Tree.Temp

open Core

type exp = Ex of Tree.exp | Nx of Tree.stm | Cx of (Tree.label * Tree.label -> Tree.stm)
type _level = {level: level; frame: Frame.frame}
and level = Outermost | Level of _level * unit ref
type access = level * Frame.access

let outermost = Outermost

let fragments = ref []

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

let getResult () = !fragments

let seq stms =
    let module T = Tree in
    
    let rec aux stms res =
        match stms with
        | [] -> assert(false)
        | [ stm ] -> T.SEQ (res, stm)
        | stm::stms' -> aux stms' (T.SEQ (res, stm))
    in

    match stms with
    | [] -> assert(false)
    | [ stm ] -> stm
    | stm::stms' -> aux stms' stm


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
    | A.EqOp -> Cx (fun (t, f) -> (T.CJUMP (T.EQ, leftEx, rightEx, t, f)))
    | A.NeqOp -> Cx (fun (t, f) -> (T.CJUMP (T.NE, leftEx, rightEx, t, f)))
    | A.LtOp -> Cx (fun (t, f) -> (T.CJUMP (T.LT, leftEx, rightEx, t, f)))
    | A.LeOp -> Cx (fun (t, f) -> (T.CJUMP (T.LE, leftEx, rightEx, t, f)))
    | A.GtOp -> Cx (fun (t, f) -> (T.CJUMP (T.LE, rightEx, leftEx, t, f)))
    | A.GeOp -> Cx (fun (t, f) -> (T.CJUMP (T.LT, rightEx, leftEx, t, f)))

let transSeq exps =
    let module T = Tree in
    
    let rec aux exps res =
        match exps with
        | [] -> assert(false)
        | [ exp ] -> T.ESEQ (res, unEx exp)
        | exp::exps' -> aux exps' (T.SEQ (res,unNx exp))
    in

    match exps with 
    | [] -> assert(false)
    | [ exp ] -> exp
    | exp::exps' -> Ex (aux exps' (unNx exp))

let transString lit =
    let module T = Tree in
    let lab = Temp.newlabel () in
    fragments := (Frame.STRING (lab, lit))::!fragments;
    Ex (T.NAME (lab))

let level_equal left right =
    match (left, right) with 
    | (Outermost, Outermost) -> true
    | (Outermost, _) | (_, Outermost) -> false
    | (Level (_, left), Level (_, right)) -> left = right

let follow_static_link declvl uselvl =
    
    let module T = Tree in
    let rec aux lvl fp =
        if level_equal declvl lvl then
            fp
        else
            match lvl with
            | Outermost -> assert(false)
            | Level (parent, _) ->
            begin
                let sl = Frame.formals parent.frame |> List.hd_exn in
                aux parent.level (Frame.exp (sl, fp))
            end
    in

    aux uselvl (T.TEMP Frame.fp)

let transCall (declvl, uselvl, lbl, args, rtype) = 
    let module T = Tree in
    let sl = follow_static_link declvl uselvl in
    let call = T.CALL ((T.NAME lbl), sl::(List.map args ~f:unEx)) in
    match rtype with
    | Types.UNIT -> Nx (T.EXP call)
    | _ -> Ex call

let transRecord fldxp =
    let module T = Tree in

    let r = Temp.newtemp () in

    let init woffset xp =
        let xp = match xp with Some xp -> unEx xp | None -> T.CONST 0 in
        T.MOVE (T.MEM (T.BINOP (T.PLUS, (T.TEMP r), (T.CONST (woffset * Frame.wordSize)))), xp)
    in

    let alloc = T.MOVE ((T.TEMP r), (T.CALL ((T.NAME (Temp.namedlabel "malloc")), [ T.CONST ((List.length fldxp) * Frame.wordSize) ]))) in        
    let inits = List.mapi fldxp ~f:init in
    
    Ex (T.ESEQ (seq (alloc::inits), T.TEMP r))

let transVar ((declvl ,  access), uselvl) = 
    let module T = Tree in 
    let sl = follow_static_link declvl uselvl in
    Ex (Frame.exp (access, sl))
    
let toDo () = Ex (Tree.CONST 0)

end