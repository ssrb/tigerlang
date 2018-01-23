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
val transOp: Absyn.oper * exp * exp * Types.ty -> exp
val transSeq: exp list -> exp
val transString: string -> exp
val transCall: level * level * Temp.label * exp list * Types.ty -> exp
val transRecord: exp option list -> exp
val transAssign: exp * exp -> exp
val transIf: exp * exp * exp option -> exp
val transWhile: exp * exp * Temp.label -> exp
val transFor: access * exp * exp * exp * Temp.label -> exp
val transBreak: Temp.label -> exp
val transLet: exp list * exp -> exp
val transArray: exp * exp -> exp
val transVar: access * level -> exp
val transField: exp * int -> exp
val transSubscript: exp * exp -> exp
val procEntryExit: level:level -> body:exp -> unit

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

module T = Tree

let seq stms =
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
    match exp with 
    | Ex ex -> T.EXP ex
    | Nx nx -> nx
    | Cx cx -> 
        let l = Temp.newlabel() in
        seq [cx(l, l); T.LABEL l]

let unCx exp = 
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

let transOp (op, left, right, ty) =
    let module A = Absyn in
    let left = unEx left in
    let right = unEx right in
    if ty = Types.STRING then
        let r = Temp.newtemp () in
        match op with
        | A.EqOp -> Cx (fun (t, f) ->  seq [
            T.MOVE ((T.TEMP r), (T.CALL ((T.NAME (Temp.namedlabel "stringCompare"), [left; right]))));
            T.CJUMP (T.EQ, (T.TEMP r), (T.CONST 0), t, f) ])
        | A.NeqOp -> Cx (fun (t, f) ->  seq [
            T.MOVE ((T.TEMP r), (T.CALL ((T.NAME (Temp.namedlabel "stringCompare"), [left; right]))));
            T.CJUMP (T.NE, (T.TEMP r), (T.CONST 0), t, f) ])
        | A.LtOp -> Cx (fun (t, f) ->  seq [
            T.MOVE ((T.TEMP r), (T.CALL ((T.NAME (Temp.namedlabel "stringCompare"), [left; right]))));
            T.CJUMP (T.LT, (T.TEMP r), (T.CONST 0), t, f) ])
        | A.LeOp -> Cx (fun (t, f) ->  seq [
            T.MOVE ((T.TEMP r), (T.CALL ((T.NAME (Temp.namedlabel "stringCompare"), [left; right]))));
            T.CJUMP (T.LE, (T.TEMP r), (T.CONST 0), t, f) ])
        | A.GtOp -> Cx (fun (t, f) ->  seq [
            T.MOVE ((T.TEMP r), (T.CALL ((T.NAME (Temp.namedlabel "stringCompare"), [right; left]))));
            T.CJUMP (T.LT, (T.TEMP r), (T.CONST 0), t, f) ])
        | A.GeOp -> Cx (fun (t, f) ->  seq [
            T.MOVE ((T.TEMP r), (T.CALL ((T.NAME (Temp.namedlabel "stringCompare"), [right; left]))));
            T.CJUMP (T.LE, (T.TEMP r), (T.CONST 0), t, f) ])
        | _ -> assert(false)
    else
        match op with 
        | A.PlusOp -> Ex (T.BINOP (T.PLUS, left, right))
        | A.MinusOp -> Ex (T.BINOP (T.MINUS, left, right))
        | A.MulOp -> Ex (T.BINOP (T.MUL, left, right))
        | A.DivOp -> Ex (T.BINOP (T.DIV, left, right))
        | A.EqOp -> Cx (fun (t, f) -> (T.CJUMP (T.EQ, left, right, t, f)))
        | A.NeqOp -> Cx (fun (t, f) -> (T.CJUMP (T.NE, left, right, t, f)))
        | A.LtOp -> Cx (fun (t, f) -> (T.CJUMP (T.LT, left, right, t, f)))
        | A.LeOp -> Cx (fun (t, f) -> (T.CJUMP (T.LE, left, right, t, f)))
        | A.GtOp -> Cx (fun (t, f) -> (T.CJUMP (T.LT, right, left, t, f)))
        | A.GeOp -> Cx (fun (t, f) -> (T.CJUMP (T.LE, right, left, t, f)))

let transSeq exps =
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
    let lab = Temp.newlabel () in
    fragments := (Frame.STRING (lab, lit))::!fragments;
    Ex (T.NAME (lab))

let level_equal left right =
    match (left, right) with 
    | (Outermost, Outermost) -> true
    | (Outermost, _) | (_, Outermost) -> false
    | (Level (_, left), Level (_, right)) -> left = right

let follow_static_link declvl uselvl =
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
    let sl = follow_static_link declvl uselvl in
    let call = T.CALL ((T.NAME lbl), sl::(List.map args ~f:unEx)) in
    match rtype with
    | Types.UNIT -> Nx (T.EXP call)
    | _ -> Ex call

let transRecord fldxp =
    let r = Temp.newtemp () in
    let init woffset xp =
        let xp = match xp with Some xp -> unEx xp | None -> T.CONST 0 in
        T.MOVE (T.MEM (T.BINOP (T.PLUS, (T.TEMP r), (T.CONST (woffset * Frame.wordSize)))), xp)
    in
    let alloc = T.MOVE ((T.TEMP r), (T.CALL ((T.NAME (Temp.namedlabel "malloc")), [ T.CONST ((List.length fldxp) * Frame.wordSize) ]))) in        
    let inits = List.mapi fldxp ~f:init in    
    Ex (T.ESEQ (seq (alloc::inits), T.TEMP r))

let transAssign (left, right) =
    Nx (T.MOVE ((unEx left), (unEx right)))

let transVar ((declvl ,  access), uselvl) =
    Ex (Frame.exp (access, (follow_static_link declvl uselvl)))

let transIf (test, then', else') =
    let t = Temp.newlabel () in
    let j = Temp.newlabel () in
    match else' with 
    | Some else' ->
    begin
        let f = Temp.newlabel () in
        match (then', else') with
        | (Nx then' , Nx else') ->
            Nx (seq [
                (unCx test) (t, f);
                T.LABEL t;
                then';
                T.JUMP ((T.NAME j), [j]);
                T.LABEL f;
                else';
                T.JUMP ((T.NAME j), [j]);
                T.LABEL j ])

        | (Cx _ , _) | (_ , Cx _) ->
        begin
            let t1 = Temp.newlabel () in
            let r = Temp.newtemp () in
            let optimizeCx exp = 
                match exp with
                | Cx exp -> [ exp(t1, j) ]
                | _ -> [ T.MOVE ((T.TEMP r), (unEx exp)); T.JUMP ((T.NAME j), [j])]
            in
            Ex (T.ESEQ (seq ([
                T.MOVE ((T.TEMP r), (T.CONST 0));
                (unCx test) (t, f);
                T.LABEL t] 
                @ (optimizeCx then')
                @ [ T.LABEL f ] 
                @ (optimizeCx else')
                @ [T.LABEL t1;
                T.MOVE ((T.TEMP r), (T.CONST 1));
                T.LABEL j]),
                T.TEMP r))
        end
        | _ ->
            let r = Temp.newtemp () in
            Ex (T.ESEQ (seq [
                (unCx test) (t, f);
                T.LABEL t;
                T.MOVE ((T.TEMP r), (unEx then'));
                T.JUMP ((T.NAME j), [j]);
                T.LABEL f;
                T.MOVE ((T.TEMP r), (unEx else'));
                T.JUMP ((T.NAME j), [j]);
                T.LABEL j ],
                T.TEMP r))

    end
    | None ->
    begin
        match then' with 
        | Cx then' ->
            Nx (seq [
            (unCx test) (t, j);
            T.LABEL t;
            then' (j, j);
            T.LABEL j ])

        | _ ->
            Nx (seq [
                (unCx test) (t, j);
                T.LABEL t;
                unNx then';
                T.JUMP ((T.NAME j), [j]);
                T.LABEL j ])
    end

let transWhile (test, body, finish) =
    let r = Temp.newtemp () in
    let start = Temp.newlabel () in
    let work = Temp.newlabel () in
    Ex (T.ESEQ (seq [
        T.LABEL start;
        (unCx test) (work, finish);
        T.LABEL work;
        T.MOVE ((T.TEMP r), (unEx body));
        T.JUMP ((T.NAME start), [start]);
        T.LABEL finish ],
        T.TEMP r))
 
let transFor (var, lo, hi, body, finish) =
    let var = Frame.exp (snd var, (T.TEMP Frame.fp)) in
    let r = Temp.newtemp () in
    let work = Temp.newlabel () in
    let increment = Temp.newlabel () in
    Ex (T.ESEQ (seq [
        unNx lo;
        T.CJUMP (T.GT, var, (unEx hi), finish, work);
        T.LABEL work;
        T.MOVE ((T.TEMP r), (unEx body));
        T.CJUMP (T.LE, var, (unEx hi), increment, finish);
        T.LABEL increment;
        T.MOVE (var, (T.BINOP (T.PLUS, (T.TEMP r), (T.CONST 1))));
        T.JUMP ((T.NAME work), [work]);
        T.LABEL finish ],
        T.TEMP r))

let transBreak label =
    Nx (Tree.JUMP (Tree.NAME label, [label]))

let transLet (inits, body) =
    Ex (T.ESEQ (inits |> List.map ~f:unNx |> seq, unEx body))

let transArray (size, init) =
    let r = Temp.newtemp () in
    let alloc = T.MOVE ((T.TEMP r), (T.CALL ((T.NAME (Temp.namedlabel "malloc")), [ T.BINOP (T.MUL, (unEx size), (T.CONST Frame.wordSize)) ]))) in
    let init = T.EXP (T.CALL ((T.NAME (Temp.namedlabel "initArray")), [ (T.TEMP r); (unEx init) ])) in
    Ex (T.ESEQ (seq [alloc; init], T.TEMP r))

let transField (var, fidx) =
    Ex (T.MEM (T.BINOP (T.PLUS, (unEx var), (T.BINOP (T.MUL, (T.CONST fidx), (T.CONST Frame.wordSize))))))

let transSubscript (var, sub) = 
    Ex (T.MEM (T.BINOP (T.PLUS, (unEx var), (T.BINOP (T.MUL, (unEx sub), (T.CONST Frame.wordSize))))))

let procEntryExit ~level ~body =
    match level with
    | Level (level, _) -> 
    begin
      fragments := (Frame.PROC {body = Frame.procEntryExit1 (level.frame, T.MOVE(T.TEMP Frame.rv, (unEx body))); frame = level.frame})::!fragments
    end
    | Outermost -> assert(false)
    
end
