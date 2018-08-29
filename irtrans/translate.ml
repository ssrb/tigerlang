module type T = sig

module Frame: Frame.T
module Temp = Frame.Temp

type exp
type level
type access

val outermost: level
val newLevel: parent:level -> name:Temp.label -> formals:bool list -> level
val formals: level -> access list
val allocLocal: level -> bool -> access
val getResult: unit -> Frame.frag list

val transNop: unit -> exp
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
val transVar: access * level * Types.ty -> exp
val transField: exp * int * Types.ty -> exp
val transSubscript: exp * exp * Types.ty -> exp
val procEntryExit: level:level -> body:exp -> unit

end

module F = functor(Frame : Frame.T) ->
struct

module Frame = Frame
module Temp = Frame.Temp
module Tree = Frame.Tree

open Tree
open Core

type exp = Ex of Tree.exp | Nx of Tree.stm | Cx of (label * label -> Tree.stm)
type _level = {level: level; frame: Frame.frame}
and level = Outermost | Level of _level * unit ref [@@deriving sexp]
type access = level * Frame.access

let outermost = Outermost

let fragments = ref []

let newLevel ~parent ~name ~formals =
Level ({level = parent; frame = Frame.newFrame ~name ~formals:(true::formals)}, ref ())

let formals lvl = 
    match lvl with
    | Outermost -> assert false
    | Level ({frame; _}, _) -> frame |> Frame.formals |> List.tl_exn |> List.map ~f:(fun acc -> (lvl, acc))

let allocLocal lvl escape = 
    match lvl with
    | Outermost -> assert false
    | Level ({frame; _}, _) -> (lvl, Frame.allocLocal frame escape)

let getResult () = !fragments

let unEx exp = 
    match exp with 
    | Ex ex -> ex
    | Nx nx -> { t = ESEQ (nx, { t = CONST 0; addr = false }); addr = false }
    | Cx cx -> 
        let r = Temp.newtemp () in
        let t = Temp.newlabel () in
        let f = Temp.newlabel () in
        { t = ESEQ(seq [
            MOVE({ t = TEMP r; addr = false }, { t = CONST 1; addr = false }); 
            cx (t, f); 
            LABEL f; 
            MOVE({ t = TEMP r; addr = false }, { t = CONST 0; addr = false }); 
            LABEL t], 
            { t = TEMP r; addr = false }); addr = false}

let unNx exp =
    match exp with 
    | Ex ex -> EXP ex
    | Nx nx -> nx
    | Cx cx -> 
        let l = Temp.newlabel() in
        seq [cx(l, l); LABEL l]

let unCx exp =  
    match exp with 
    | Ex ex ->
    begin
        match ex.t with
        | CONST 0 -> (fun (t, f) -> JUMP ({ t = NAME f; addr = true }, [f]))
        | CONST _ -> (fun (t, f) -> JUMP ({ t = NAME t; addr = true }, [t]))
        | _ -> (fun (t, f) -> CJUMP (EQ, { t = CONST 0; addr = false }, ex, f, t))
    end
    | Nx nx -> assert false
    | Cx cx -> cx

let transNop () = Nx NOP

let transNil () = Ex { t = CONST 0; addr = true }

let transInt i = Ex { t = CONST i; addr = false }

let transOp (op, left, right, ty) =
    let module A = Absyn in
    let left = unEx left in
    let right = unEx right in
    let zero = { t = CONST 0; addr = false } in
    if ty = Types.STRING then
        match op with
        | A.EqOp -> Cx (fun (t, f) ->
            CJUMP (EQ, Frame.externalCall ("stringCompare", [left; right]), zero, t, f))
        | A.NeqOp -> Cx (fun (t, f) ->
            CJUMP (NE, Frame.externalCall ("stringCompare", [left; right]), zero, t, f))
        | A.LtOp -> Cx (fun (t, f) ->
            CJUMP (LT, Frame.externalCall ("stringCompare", [left; right]), zero, t, f))
        | A.LeOp -> Cx (fun (t, f) ->
            CJUMP (LE, Frame.externalCall ("stringCompare", [left; right]), zero, t, f))
        | A.GtOp -> Cx (fun (t, f) ->
            CJUMP (LT, Frame.externalCall ("stringCompare", [right; left]), zero, t, f))
        | A.GeOp -> Cx (fun (t, f) ->
            CJUMP (LE, Frame.externalCall ("stringCompare", [right; left]), zero, t, f))
        | _ -> assert false
    else
        let addr = left.addr || right.addr in
        match op with 
        | A.PlusOp -> Ex { t = BINOP (PLUS, left, right); addr }
        | A.MinusOp -> Ex { t = BINOP (MINUS, left, right); addr }
        | A.MulOp -> Ex { t = BINOP (MUL, left, right); addr }
        | A.DivOp -> Ex { t = BINOP (DIV, left, right); addr }
        | A.EqOp -> Cx (fun (t, f) -> (CJUMP (EQ, left, right, t, f)))
        | A.NeqOp -> Cx (fun (t, f) -> (CJUMP (NE, left, right, t, f)))
        | A.LtOp -> Cx (fun (t, f) -> (CJUMP (LT, left, right, t, f)))
        | A.LeOp -> Cx (fun (t, f) -> (CJUMP (LE, left, right, t, f)))
        | A.GtOp -> Cx (fun (t, f) -> (CJUMP (LT, right, left, t, f)))
        | A.GeOp -> Cx (fun (t, f) -> (CJUMP (LE, right, left, t, f)))

let transSeq exps =
    let rec aux exps res =
        match exps with
        | [] -> assert false
        | [ exp ] -> 
            let exp = unEx exp in
            Ex { t = ESEQ (res, exp); addr = exp.addr } 
        | exp::exps' -> aux exps' (SEQ (res, unNx exp))
    in
    match exps with 
    | [] -> assert false
    | [ exp ] -> exp
    | exp::exps' -> aux exps' (unNx exp)

let transString lit =
    let lab = Temp.newlabel () in
    fragments := (Frame.STRING (lab, lit))::!fragments;
    Ex { t = NAME lab; addr = true }

let level_equal left right =
    match (left, right) with 
    | (Outermost, Outermost) -> true
    | (Outermost, _) | (_, Outermost) -> false
    | (Level (_, left), Level (_, right)) -> phys_equal left right

let follow_static_link declvl uselvl =
    let rec aux lvl fp =
        if level_equal declvl lvl then
            fp
        else
            match lvl with
            | Outermost -> assert false
            | Level (parent, _) ->
            begin
                let sl = Frame.formals parent.frame |> List.hd_exn in
                aux parent.level (Frame.exp (sl, fp))
            end
    in
    aux uselvl { t = TEMP Frame.fp; addr = true }

let transCall (declvl, uselvl, lbl, args, rtype) = 
    let args = List.map args ~f:unEx in
    let addr = Types.is_addr rtype in
    let call = 
        match declvl with
        | Outermost -> { (Frame.externalCall (Symbol.name lbl, args)) with addr }
        | _ -> { t = CALL ({ t = NAME lbl; addr = true }, (follow_static_link declvl uselvl)::args); addr }
    in 
    match rtype with
    | Types.UNIT -> Nx (EXP call)
    | _ -> Ex call

let transRecord fldxp =
    let r = { t = TEMP (Temp.newtemp ()); addr = true } in
    let init woffset xp =
        let xp = match xp with Some xp -> unEx xp | None -> { t = CONST 0; addr = false (* <= TODO *) } in
        MOVE ({ t = MEM ( { t = BINOP (PLUS, r, { t = CONST (woffset * Frame.wordSize); addr = false }); addr = true }); 
                addr = false (** <= TODO *)}, xp)
    in
    let alloc = MOVE (r, { (Frame.externalCall ("malloc", [ { t = CONST ((List.length fldxp) * Frame.wordSize); addr = false } ])) with addr = true }) in        
    let inits = List.mapi fldxp ~f:init in    
    Ex { t = ESEQ (seq (alloc::inits), r); addr = true }

let transAssign (left, right) =
    Nx (MOVE ((unEx left), (unEx right)))

let transVar ((declvl ,  access), uselvl, ty) =
    Ex  { (Frame.exp (access, (follow_static_link declvl uselvl))) with addr = Types.is_addr ty }

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
                LABEL t;
                then';
                JUMP ({ t = NAME j; addr = true }, [j]);
                LABEL f;
                else';
                JUMP ({ t = NAME j; addr = true }, [j]);
                LABEL j ])

        | (Cx _ , _) | (_ , Cx _) ->
        begin
            let t1 = Temp.newlabel () in
            let r = Temp.newtemp () in
            let optimizeCx exp = 
                match exp with
                | Cx exp -> [ exp(t1, j) ]
                | _ -> 
                    let exp = unEx exp in
                    [ MOVE ({ t = TEMP r; addr = exp.addr }, exp); JUMP ({ t = NAME j; addr = true }, [j])]
            in
            Ex { t = (ESEQ (seq ([
                MOVE ({ t = TEMP r; addr = false }, { t = CONST 0; addr = false });
                (unCx test) (t, f);
                LABEL t] 
                @ (optimizeCx then')
                @ [ LABEL f ] 
                @ (optimizeCx else')
                @ [LABEL t1;
                MOVE ({ t = TEMP r; addr = false }, { t = CONST 1; addr = false });
                LABEL j]),
                { t = TEMP r; addr = false })); addr = false }
        end
        | _ ->
            let r = Temp.newtemp () in
            let then' = unEx then' in
            let else' = unEx else' in
            assert (then'.addr = else'.addr);
            let addr = then'.addr in
            Ex { t = (ESEQ (seq [
                (unCx test) (t, f);
                LABEL t;
                MOVE ({ t = TEMP r; addr }, then');
                JUMP ({ t = NAME j; addr = true }, [j]);
                LABEL f;
                MOVE ({ t = TEMP r; addr }, else');
                JUMP ({ t = NAME j; addr = true }, [j]);
                LABEL j ],
                { t = TEMP r; addr })); addr }

    end
    | None ->
    begin
        match then' with 
        | Cx then' ->
            Nx (seq [
            (unCx test) (t, j);
            LABEL t;
            then' (j, j);
            LABEL j ])

        | _ ->
            Nx (seq [
                (unCx test) (t, j);
                LABEL t;
                unNx then';
                JUMP ({ t = NAME j; addr = true }, [j]);
                LABEL j ])
    end

let transWhile (test, body, finish) =
    let start = Temp.newlabel () in
    let work = Temp.newlabel () in
    Nx (seq [
        LABEL start;
        (unCx test) (work, finish);
        LABEL work;
        (unNx body);
        JUMP ({ t = NAME start; addr = true }, [start]);
        LABEL finish ])
 
let transFor (var, lo, hi, body, finish) =
    let var = Frame.exp (snd var, ({ t = TEMP Frame.fp; addr = true })) in
    let work = Temp.newlabel () in
    let increment = Temp.newlabel () in
    Nx (seq [
        (unNx lo);
        CJUMP (GT, var, (unEx hi), finish, work);
        LABEL work;
        (unNx body);
        CJUMP (LT, var, (unEx hi), increment, finish);
        LABEL increment;
        MOVE (var, { t = BINOP (PLUS, var, { t = CONST 1; addr = false }); addr = false });
        JUMP ({ t = NAME work; addr = true }, [work]);
        LABEL finish ])

let transBreak label =
    Nx (JUMP ({ t = NAME label; addr = true }, [label]))

let transLet (inits, body) =
    match inits with
    | [] -> body
    | _ -> 
        let body = unEx body in
        Ex { t = ESEQ (inits |> List.map ~f:unNx |> seq, body); addr = body.addr }

let transArray (size, init) =
    Ex { (Frame.externalCall ("initArray", [ (unEx size); (unEx init) ])) with addr = true }

let transField (var, fidx, ty) =
    Ex { t = MEM { t = BINOP (
        PLUS, 
        (unEx var), 
        { t = BINOP (
            MUL, 
            { t = CONST fidx; addr = false }, 
            { t = CONST Frame.wordSize; addr = false }
        ); addr = false } 
    ); addr = true }; addr = Types.is_addr ty}

let transSubscript (var, sub, ty) = 
    Ex { t = (MEM { t = BINOP (
        PLUS, 
        (unEx var), 
        { t = BINOP (
            MUL, 
            (unEx sub), 
            { t = CONST Frame.wordSize; addr = false }
        ); addr = false}
    ); addr = true}); addr = Types.is_addr ty}
    
let procEntryExit ~level ~body =
    match level with
    | Level (level, _) -> 
    begin
        let body = match body with
        | Nx nx -> nx
        | _ -> MOVE({ t = TEMP Frame.rv; addr = false (* <= TODO *) }, (unEx body))
        in
        fragments := (Frame.PROC {body = Frame.procEntryExit1 (level.frame, body); frame = level.frame})::!fragments
    end
    | Outermost -> assert false
    
end
