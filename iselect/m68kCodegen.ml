module Frame = M68kFrame
module Tree = Frame.Tree
module Assem = Frame.Assem
module Temp = Frame.Temp
module Var = Assem.Variable

open Core
open Tree

type operand = { assem: string; dst: Var.t list; src: Var.t list }

let codegen frame stm =
    let module A = Assem in
    let ilist = ref [] in
    let emit x = ilist := x::!ilist in
    let data gen = 
        let v = Var.make (Temp.newtemp(), "d") in 
        gen v; 
        v
    in
    let address gen = 
        let v = Var.make (Temp.newtemp(), "a") in 
        gen v; 
        v
    in
    let rec munchStm = function
        | NOP -> ()
 
        | MOVE(e0, e1) ->
        begin
            match e1 with
            | { t = CALL (l, args) } ->
                emitCall (l, args) e0
            | _ ->
                let dst = munchOperand e0 true in
                let src = munchOperand e1 false in
                let srcdst = mergeOperands src dst in
                match e1 with
                | { t = NAME l } ->
                    emit(A.OPER {assem = "lea.l " ^ srcdst.assem; dst = srcdst.dst; src = srcdst.src; jump = None})
                | _ ->
                    let move = if e0.addr && dst.assem = "`d0" then "movea.l " else "move.l " in
                    if srcdst.assem = "`s0,`d0" then
                        emit(A.MOVE {assem = move ^ srcdst.assem; dst = List.hd_exn srcdst.dst; src = List.hd_exn srcdst.src })
                    else
                        emit(A.OPER {assem = move ^ srcdst.assem; dst = srcdst.dst; src = srcdst.src; jump = None})
        end

        | LABEL label -> 
            emit(A.LABEL {assem = (Symbol.name label) ^ ":"; lab = label})
        
        | JUMP ({ t = NAME l }, lbls) ->
            emit(A.OPER {assem = "jmp " ^ (Symbol.name l); dst = []; src = []; jump = Some lbls})

        | CJUMP (relop, e0, e1, t, f) ->
        begin

            let relop = 
                assert(e0.addr = e1.addr);
                match (e0.t, e1.t) with
                | (CONST i, _) ->
                    if e0.addr then
                        emit(A.OPER {assem = "cmpa.l #" ^ Int.to_string i ^ ",`s0"; dst = []; src = [munchDataExp e1]; jump = None})
                    else if i <> 0 then
                        emit(A.OPER {assem = "cmpi.l #" ^ Int.to_string i ^ ",`s0"; dst = []; src = [munchDataExp e1]; jump = None})
                    else
                        emit(A.OPER {assem = "tst.l `s0"; dst = []; src = [munchDataExp e1]; jump = None});
                    Tree.commute relop
                | (_, CONST i) ->
                    if e0.addr then
                        emit(A.OPER {assem = "cmpa.l #" ^ Int.to_string i ^ ",`s0"; dst = []; src = [munchDataExp e0]; jump = None})
                    else if i <> 0 then
                        emit(A.OPER {assem = "cmpi.l #" ^ Int.to_string i ^ ",`s0"; dst = []; src = [munchDataExp e0]; jump = None})
                    else
                        emit(A.OPER {assem = "tst.l `s0"; dst = []; src = [munchDataExp e0]; jump = None});
                    relop
                | _ ->
                    if e0.addr then
                        emit(A.OPER {assem = "cmpa.l `s1,`s0"; dst = []; src = [munchDataExp e0; munchDataExp e1]; jump = None})
                    else
                        emit(A.OPER {assem = "cmp.l `s1,`s0"; dst = []; src = [munchDataExp e0; munchDataExp e1]; jump = None});
                    relop
            in

            match relop with 
            | EQ -> 
                emit(A.OPER {assem = "beq " ^ (Symbol.name t); dst = []; src = []; jump = Some [t; f]})
            | NE ->
                emit(A.OPER {assem = "bne " ^ (Symbol.name t); dst = []; src = []; jump = Some [t; f]})
            | LT | ULT ->
                emit(A.OPER {assem = "blt " ^ (Symbol.name t); dst = []; src = []; jump = Some [t; f]})
            | GT | UGT ->
                emit(A.OPER {assem = "bgt " ^ (Symbol.name t); dst = []; src = []; jump = Some [t; f]})
            | LE | ULE ->
                emit(A.OPER {assem = "ble " ^ (Symbol.name t); dst = []; src = []; jump = Some [t; f]})
            | GE | UGE ->
                emit(A.OPER {assem = "bge " ^ (Symbol.name t); dst = []; src = []; jump = Some [t; f]})
        end

        | EXP e -> ignore(if e.addr then munchAddrExp e else munchDataExp e)

        | stm -> 
            stm
            |> Tree.sexp_of_stm
            |> Sexp.output_hum Out_channel.stdout;
            assert(false)

    and emitCall (l, args) r = 

        let saverestore = Frame.callersaves |> List.map ~f:(fun(reg : Frame.Assem.Variable.t) ->
            let addr = reg.regclass = "a" in
            let memory = Frame.exp ((Frame.allocLocal frame false), { t = TEMP Frame.fp; addr = true }, addr) in
            (MOVE (memory, { t = TEMP reg.temp; addr } ) , MOVE ({ t = TEMP reg.temp; addr }, memory))
        )
        in

        let nargs = List.length args in
        
        saverestore |> List.iter ~f:(fun (s, _) -> munchStm s);

        (* We pass everything on the stack *)
        if nargs > 0 then
            emit(A.OPER {assem = "suba.l #" ^ (Int.to_string (4 * nargs)) ^ ",sp"; dst = []; src = []; jump = None});

        args |> List.iteri ~f:(fun i a -> 
            emit(A.OPER {assem = "move.l `s0,+" ^ (Int.to_string (4 * i)) ^ "(sp)"; dst = []; src = [ munchDataExp a ]; jump = None})
        );

        (match l with
        | { t = NAME l } -> emit(A.OPER {assem = "jsr " ^ (Symbol.name l); dst = []; src = []; jump = None})
        | _ -> emit(A.OPER {assem = "jsr (`s0)"; dst = []; src = [ munchAddrExp l ]; jump = None}));

        if nargs > 0 then
            emit(A.OPER {assem = "adda.l #" ^ (Int.to_string (4 * nargs)) ^ ",sp"; dst = []; src = []; jump = None});

        munchStm (MOVE (r , { t = TEMP Frame.rv; addr = false } ) );

        saverestore |> List.iter ~f:(fun (_, r) -> munchStm r)
    
    and munchDataExp = function

        | { t = BINOP (op, e0, e1) } -> 
        begin
            match op with 
            | PLUS ->
                begin
                    match (e0, e1) with
                    | ({ t = CONST i }, { t = MEM e0 }) | ({ t = MEM e0 }, { t = CONST i }) ->
                        let s0 = munchAddrExp e0 in
                        data(fun r -> 
                            emit(A.OPER {assem = "move.l (`s0),`d0"; dst = [r]; src = [s0]; jump = None});
                            emit(A.OPER {assem = "addi.l #" ^ (Int.to_string i) ^ ",`d0"; dst = [r]; src = [r]; jump = None}))
                    | ({ t = CONST i }, e0) | (e0, { t = CONST i }) ->
                        let s0 = munchDataExp e0 in
                        data(fun r -> 
                            emit(A.MOVE {assem = "move.l `s0,`d0"; dst = r; src = s0});
                            emit(A.OPER {assem = "addi.l #" ^ (Int.to_string i) ^ ",`d0"; dst = [r]; src = [r]; jump = None}))
                    | ({ t = MEM e0 }, e1) | (e1, { t = MEM e0}) ->
                        let s0 = munchAddrExp e0 in
                        let s1 = munchDataExp e1 in
                        data(fun r -> 
                            emit(A.MOVE {assem = "move.l `s0,`d0"; dst = r; src = s1});
                            emit(A.OPER {assem = "add.l (`s0),`d0"; dst = [r]; src = [s0; r]; jump = None}))
                    | _ ->
                        let s0 = munchDataExp e0 in
                        let s1 = munchDataExp e1 in
                        data(fun r -> 
                            emit(A.MOVE {assem = "move.l `s0,`d0"; dst = r; src = s0});
                            emit(A.OPER {assem = "add.l `s0,`d0"; dst = [r]; src = [s1; r]; jump = None}))
                end
            | MINUS ->
                begin
                    match (e0, e1) with
                    | ({ t = MEM e0 }, { t = CONST i }) ->
                        let s0 = munchAddrExp e0 in
                        data(fun r -> 
                            emit(A.OPER {assem = "move.l (`s0),`d0"; dst = [r]; src = [s0]; jump = None});
                            emit(A.OPER {assem = "subi.l #" ^ (Int.to_string i) ^ ",`d0"; dst = [r]; src = [r]; jump = None}))
                    | (e0, { t = CONST i }) ->
                        let s0 = munchDataExp e0 in
                        data(fun r -> 
                            emit(A.MOVE {assem = "move.l `s0,`d0"; dst = r; src = s0});
                            emit(A.OPER {assem = "subi.l #" ^ (Int.to_string i) ^ ",`d0"; dst = [r]; src = [r]; jump = None}))
                    | ({ t = MEM e0 }, e1) ->
                        let s0 = munchAddrExp e0 in
                        let s1 = munchDataExp e1 in
                        data(fun r -> 
                            emit(A.OPER {assem = "move.l `(s0),`d0"; dst = [r]; src = [s0]; jump = None});
                            emit(A.OPER {assem = "sub.l `s0,`d0"; dst = [r]; src = [s1; r]; jump = None}))
                    | (e0, { t = MEM e1}) ->
                        let s0 = munchDataExp e0 in
                        let s1 = munchAddrExp e1 in
                        data(fun r -> 
                            emit(A.MOVE {assem = "move.l `s0,`d0"; dst = r; src = s0});
                            emit(A.OPER {assem = "sub.l (`s0),`d0"; dst = [r]; src = [s1; r]; jump = None}))
                    | _ ->
                        let s0 = munchDataExp e0 in
                        let s1 = munchDataExp e1 in
                        data(fun r -> 
                            emit(A.MOVE {assem = "move.l `s0,`d0"; dst = r; src = s0});
                            emit(A.OPER {assem = "sub.l `s0,`d0"; dst = [r]; src = [s1; r]; jump = None}))
                end
            | MUL ->
                let s0 = munchDataExp e0 in
                let s1 = munchDataExp e1 in
                 data(fun r -> 
                    emit(A.MOVE {assem = "move.l `s0,`d0"; dst = r; src = s0});
                    emit(A.OPER {assem = "muls.w `s0,`d0"; dst = [r]; src = [s1; r]; jump = None})) 
            | DIV ->
                let s0 = munchDataExp e0 in
                let s1 = munchDataExp e1 in
                 data(fun r -> 
                    emit(A.MOVE {assem = "move.l `s0,`d0"; dst = r; src = s0});
                    emit(A.OPER {assem = "divs.w `s0,`d0"; dst = [r]; src = [s1; r]; jump = None})) 
            | AND ->
                let s0 = munchDataExp e0 in
                let s1 = munchDataExp e1 in
                 data(fun r -> 
                    emit(A.MOVE {assem = "move.l `s0,`d0"; dst = r; src = s0});
                    emit(A.OPER {assem = "and.l `s0,`d0"; dst = [r]; src = [s1; r]; jump = None})) 
            | OR ->
                let s0 = munchDataExp e0 in
                let s1 = munchDataExp e1 in
                 data(fun r -> 
                    emit(A.MOVE {assem = "move.l `s0,`d0"; dst = r; src = s0});
                    emit(A.OPER {assem = "or.l `s0,`d0"; dst = [r]; src = [s1; r]; jump = None})) 
            | LSHIFT ->
                let s0 = munchDataExp e0 in
                let s1 = munchDataExp e1 in
                 data(fun r -> 
                    emit(A.MOVE {assem = "move.l `s0,`d0"; dst = r; src = s0});
                    emit(A.OPER {assem = "lsl.l `s0,`d0"; dst = [r]; src = [s1; r]; jump = None})) 
            | RSHIFT ->
                let s0 = munchDataExp e0 in
                let s1 = munchDataExp e1 in
                 data(fun r -> 
                    emit(A.MOVE {assem = "move.l `s0,`d0"; dst = r; src = s0});
                    emit(A.OPER {assem = "lsr.l `s0,`d0"; dst = [r]; src = [s1; r]; jump = None})) 
            | ARSHIFT ->
                let s0 = munchDataExp e0 in
                let s1 = munchDataExp e1 in
                 data(fun r -> 
                    emit(A.MOVE {assem = "move.l `s0,`d0"; dst = r; src = s0});
                    emit(A.OPER {assem = "asr.l `s0,`d0"; dst = [r]; src = [s1; r]; jump = None})) 
            | XOR ->
                let s0 = munchDataExp e0 in
                let s1 = munchDataExp e1 in
                 data(fun r -> 
                    emit(A.MOVE {assem = "move.l `s0,`d0"; dst = r; src = s0});
                    emit(A.OPER {assem = "eor.l `s0,`d0"; dst = [r]; src = [s1; r]; jump = None})) 
        end
        | { t = MEM { t = BINOP (PLUS, e0, { t = CONST i } ) } }
        | { t = MEM { t = BINOP (PLUS, { t = CONST i }, e0) } } -> data(fun r -> emit(A.OPER {assem = "move.l " ^ (Int.to_string i) ^ "(`s0),`d0"; dst = [r]; src = [munchAddrExp e0]; jump = None}))
        | { t = MEM { t = BINOP (MINUS, e0, { t = CONST i }) } } -> data(fun r -> emit(A.OPER {assem = "move.l " ^ (Int.to_string ~-i) ^ "(`s0),`d0"; dst = [r]; src = [munchAddrExp e0]; jump = None}))
        | { t = MEM { t = CONST i } } -> data(fun r -> emit(A.OPER {assem = "move.l " ^ Int.to_string i ^ ",`d0"; dst = [r]; src = []; jump = None}))
        | { t = MEM e0 } -> data(fun r -> emit(A.OPER {assem = "move.l (`s0),`d0"; dst = [r]; src = [munchAddrExp e0]; jump = None}))
        | { t = TEMP temp; addr } -> Var.make (temp, if addr then "a" else "d")
        
        | { t = NAME l } -> address(fun r -> emit(A.OPER {assem = "lea.l " ^ (Symbol.name l) ^ ",`d0" ; dst = [r]; src = []; jump = None}))
        
        | { t = CONST i } -> data(fun r -> emit(A.OPER {assem = "move.l #" ^ (Int.to_string i) ^ ",`d0"; dst = [r]; src = []; jump = None}))
 
        | { t = CALL (l, args) } -> data(fun r -> emitCall (l, args) { t = TEMP r.temp; addr = false })
            
        | exp -> 
            exp
            |> Tree.sexp_of_exp
            |> Sexp.output_hum Out_channel.stderr;
            assert(false)

    and munchAddrExp addrexp = 
        if not addrexp.addr then
             addrexp |> Tree.sexp_of_exp |> Sexp.output_hum Out_channel.stderr;

        match addrexp with
        | { t = BINOP (op, e0, e1) } ->
        begin
            match op with 
            | PLUS ->
                begin
                    match (e0, e1) with
                    | ({ t = CONST i }, e0) | (e0, { t = CONST i }) ->
                        let s0 = munchDataExp e0 in
                        address(fun r -> 
                            emit(A.OPER {assem = "movea.l `s0,`d0"; dst = [r]; src = [s0]; jump = None});
                            emit(A.OPER {assem = "adda.l #" ^ (Int.to_string i) ^ ",`d0"; dst = [r]; src = [r]; jump = None}))
                    | ({ t = MEM e0 }, e1) | (e1, { t = MEM e0 }) ->
                        let s0 = munchAddrExp e0 in
                        let s1 = munchDataExp e1 in
                        address(fun r -> 
                            emit(A.OPER {assem = "movea.l `s0,`d0"; dst = [r]; src = [s1]; jump = None});
                            emit(A.OPER {assem = "adda.l (`s0),`d0"; dst = [r]; src = [s0; r]; jump = None}))
                    | _ ->
                        let s0 = munchDataExp e0 in
                        let s1 = munchDataExp e1 in
                        address(fun r -> 
                            emit(A.OPER {assem = "movea.l `s0,`d0"; dst = [r]; src = [s0]; jump = None});
                            emit(A.OPER {assem = "adda.l `s0,`d0"; dst = [r]; src = [s1; r]; jump = None}))
                end
            | MINUS ->
                let s0 = munchDataExp e0 in
                let s1 = munchDataExp e1 in
                 address(fun r -> 
                    emit(A.OPER {assem = "movea.l `s0,`d0"; dst = [r]; src = [s0]; jump = None});
                    emit(A.OPER {assem = "suba.l `s0,`d0"; dst = [r]; src = [s1; r]; jump = None})) 
            
            | _ -> assert(false)
        end
        | { t = MEM { t = BINOP (PLUS, e0, { t = CONST i } ) } }
        | { t = MEM { t = BINOP (PLUS, { t = CONST i }, e0) } } -> address(fun r -> emit(A.OPER {assem = "movea.l " ^ (Int.to_string i) ^ "(`s0),`d0"; dst = [r]; src = [munchAddrExp e0]; jump = None}))
        | { t = MEM { t = BINOP (MINUS, e0, { t = CONST i }) } } -> address(fun r -> emit(A.OPER {assem = "movea.l " ^ (Int.to_string ~-i) ^ "(`s0),`d0"; dst = [r]; src = [munchAddrExp e0]; jump = None}))
        | { t = MEM { t = CONST i } } -> address(fun r -> emit(A.OPER {assem = "movea.l "^ Int.to_string i ^ ",`d0"; dst = [r]; src = []; jump = None}))
        | { t = MEM e0 } -> address(fun r -> emit(A.OPER {assem = "movea.l (`s0),`d0"; dst = [r]; src = [munchAddrExp e0]; jump = None}))
        | { t = TEMP temp; addr } -> Var.make (temp, if addr then "a" else "d")
        
        | { t = NAME l } -> address(fun r -> emit(A.OPER {assem = "lea.l " ^ (Symbol.name l) ^ ",`d0" ; dst = [r]; src = []; jump = None}))
        
        | { t = CONST i } -> address(fun r -> emit(A.OPER {assem = "lea.l #" ^ (Int.to_string i) ^ ",`d0"; dst = [r]; src = []; jump = None}))
 
        | { t = CALL (l, args) } -> address(fun r -> emitCall (l, args) { t = TEMP r.temp; addr = true })

        | _ -> 
            addrexp
            |> Tree.sexp_of_exp
            |> Sexp.output_hum Out_channel.stderr;
            assert(false)
        
        and munchOperand op dst =
            match op with
            | { t = MEM { t = BINOP (PLUS, e, { t = CONST j } ) } }
            | { t = MEM { t = BINOP (PLUS, { t = CONST j }, e) } } ->
                { assem = (Int.to_string j) ^ "(`s0)"; dst = []; src = [munchAddrExp e] }
            | { t = MEM { t = BINOP (MINUS, e, { t = CONST j } ) } } ->
                { assem = (Int.to_string ~-j) ^ "(`s0)"; dst = []; src = [munchAddrExp e] }
            | { t = MEM { t = CONST i } } ->
                { assem = (Int.to_string i); dst = []; src = [] }
            | { t = MEM e } ->
                { assem = "(`s0)"; dst = []; src = [munchAddrExp e] }
            | { t = CONST j } ->
                { assem = "#" ^ (Int.to_string j); dst = []; src = [] }
            | { t = NAME l } ->
                { assem = (Symbol.name l); dst = []; src=  [] }
            | _ ->
                let e = if op.addr then munchAddrExp op else munchDataExp op in
                if dst then
                    { assem = "`d0"; dst = [ e ]; src = [] }
                else
                    { assem = "`s0"; dst = []; src = [ e ] }

        and mergeOperands src dst = 
            let renumber assem s d =
                let atoi c = (int_of_char c) - (int_of_char '0') in
                let rec f = function
                    | '`'::'s'::c::rest -> ("`s" ^ (Int.to_string ((atoi c) + s))) ^ (f rest)
                    | '`'::'d'::c::rest -> ("`d" ^ (Int.to_string ((atoi c) + d))) ^ (f rest)
                    | c::rest -> (String.of_char c) ^ (f rest)
                    | [] -> ""
                in
                assem |> String.to_list |> f
            in    
            let src' = src.src @ dst.src in
            let dst' = src.dst @ dst.dst in
            { assem = src.assem ^ "," ^ (renumber dst.assem (List.length src.src) (List.length src.dst)); dst = dst'; src = src' }


    in 
    munchStm stm;
    List.rev(!ilist)
