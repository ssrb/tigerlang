module Frame = M68kFrame
module Tree = Frame.Tree
module Temp = Frame.Temp
module Assem = Assem.F(M68kTemp)

open Core

module T = Tree

let codegen frame stm =
    let module A = Assem in
    let ilist = ref [] in
    let emit x = ilist := x::!ilist in
    let data gen = 
        let t = (T.Temp.newtemp(), "d") in 
        gen t; 
        t
    in
    let address gen = 
        let t = (T.Temp.newtemp(), "a") in 
        gen t; 
        t
    in
    let rec munchStm = function
    (* Data movememt *)
        
        | T.MOVE (T.MEM (T.BINOP (T.PLUS, e0, (T.CONST i))), e1)
        | T.MOVE (T.MEM (T.BINOP (T.PLUS, (T.CONST i), e0)), e1) ->
        begin
            match e1 with
            | T.MEM (T.BINOP (T.PLUS, e1, (T.CONST j)))
            | T.MEM (T.BINOP (T.PLUS, (T.CONST j), e1)) ->
                if i = j then
                    emit(A.MOVE {assem = "move.l " ^ (Int.to_string (j / 4)) ^ "(`s0)," ^ (Int.to_string (i / 4)) ^ "(`d0)"; dst = munchAddrExp e0; src = munchAddrExp e1})
                else
                    emit(A.OPER {assem = "move.l " ^ (Int.to_string (j / 4)) ^ "(`s1)," ^ (Int.to_string (i / 4)) ^ "(`s0)"; dst = []; src = [munchAddrExp e0; munchAddrExp e1]; jump = None})
  
            | T.MEM (T.BINOP (T.MINUS, e1, (T.CONST j)))
            | T.MEM (T.BINOP (T.MINUS, (T.CONST j), e1)) ->
                emit(A.OPER {assem = "move.l " ^ (Int.to_string (-j / 4)) ^ "(`s1)," ^ (Int.to_string (i / 4)) ^ "(`s0)"; dst = []; src = [munchAddrExp e0; munchAddrExp e1]; jump = None})

            | T.MEM (T.CONST i) ->
                emit(A.OPER {assem = "move.l $" ^ (Int.to_string (i / 4)) ^ ",(`s0)"; dst = []; src = [munchAddrExp e0]; jump = None})

            | T.MEM e1 ->
                emit(A.OPER {assem = "move.l (`s1)," ^ (Int.to_string (i / 4)) ^ "(`s0)"; dst = []; src = [munchAddrExp e0; munchAddrExp e1]; jump = None})

            | T.CONST i ->
                emit(A.OPER {assem = "move.l #$" ^ (Int.to_string (i / 4)) ^ ",(`s0)"; dst = []; src = [munchAddrExp e0]; jump = None})

            | e1 ->
                emit(A.OPER {assem = "move.l `s1," ^ (Int.to_string (i / 4)) ^ "(`s0)"; dst = []; src = [munchAddrExp e0; munchDataExp e1]; jump = None})
        end 

        | T.MOVE (T.MEM (T.BINOP (T.MINUS, e0, (T.CONST i))), e1)
        | T.MOVE (T.MEM (T.BINOP (T.MINUS, (T.CONST i), e0)), e1) ->
        begin
            match e1 with
            | T.MEM (T.BINOP (T.PLUS, e1, (T.CONST j)))
            | T.MEM (T.BINOP (T.PLUS, (T.CONST j), e1)) ->
                emit(A.OPER {assem = "move.l " ^ (Int.to_string (j / 4)) ^ "(`s1)," ^ (Int.to_string (-i / 4)) ^ "(`s0)"; dst = []; src = [munchAddrExp e0; munchAddrExp e1]; jump = None})
  
            | T.MEM (T.BINOP (T.MINUS, e1, (T.CONST j)))
            | T.MEM (T.BINOP (T.MINUS, (T.CONST j), e1)) ->
                if i = j then
                    emit(A.MOVE {assem = "move.l " ^ (Int.to_string (-j / 4)) ^ "(`s0)," ^ (Int.to_string (-i / 4)) ^ "(`d0)"; dst = munchAddrExp e0; src = munchAddrExp e1})
                else
                    emit(A.OPER {assem = "move.l " ^ (Int.to_string (-j / 4)) ^ "(`s1)," ^ (Int.to_string (-i / 4)) ^ "(`s0)"; dst = []; src = [munchAddrExp e0; munchAddrExp e1]; jump = None})

            | T.MEM (T.CONST i) ->
                emit(A.OPER {assem = "move.l $" ^ (Int.to_string (i / 4)) ^ ",(`s0)"; dst = []; src = [munchAddrExp e0]; jump = None})

            | T.MEM e1 ->
                emit(A.OPER {assem = "move.l (`s1)," ^ (Int.to_string (-i / 4)) ^ "(`s0)"; dst = []; src = [munchAddrExp e0; munchAddrExp e1]; jump = None})

            | T.CONST i ->
                emit(A.OPER {assem = "move.l #$" ^ (Int.to_string (i / 4)) ^ ",(`s0)"; dst = []; src = [munchAddrExp e0]; jump = None})

            | e1 ->
                emit(A.OPER {assem = "move.l `s1," ^ (Int.to_string (-i / 4)) ^ "(`s0)"; dst = []; src = [munchAddrExp e0; munchDataExp e1]; jump = None})
        end

        | T.MOVE (T.MEM e0, e1) ->
        begin
            match e1 with
            | T.MEM (T.BINOP (T.PLUS, e1, (T.CONST j)))
            | T.MEM (T.BINOP (T.PLUS, (T.CONST j), e1)) ->
                emit(A.OPER {assem = "move.l " ^ (Int.to_string (j / 4)) ^ "(`s1),(`s0)"; dst = []; src = [munchAddrExp e0; munchAddrExp e1]; jump = None})
  
            | T.MEM (T.BINOP (T.MINUS, e1, (T.CONST j)))
            | T.MEM (T.BINOP (T.MINUS, (T.CONST j), e1)) ->
                emit(A.OPER {assem = "move.l " ^ (Int.to_string (-j / 4)) ^ "(`s1),(`s0)"; dst = []; src = [munchAddrExp e0; munchAddrExp e1]; jump = None})

            | T.MEM (T.CONST i) ->
                emit(A.OPER {assem = "move.l $" ^ (Int.to_string (i / 4)) ^ ",(`s0)"; dst = []; src = [munchAddrExp e0]; jump = None})

            | T.MEM e1 ->
                if e0 <> e1 then
                    emit(A.OPER {assem = "move.l (`s1),(`s0)"; dst = []; src = [munchAddrExp e0; munchAddrExp e1]; jump = None});

            | T.CONST i ->
                emit(A.OPER {assem = "move.l #$" ^ (Int.to_string (i / 4)) ^ ",(`s0)"; dst = []; src = [munchAddrExp e0]; jump = None})

            | e1 ->
                emit(A.OPER {assem = "move.l `s1,(`s0)"; dst = []; src = [munchAddrExp e0; munchDataExp e1]; jump = None})
        end 

        | T.MOVE(e0, e1) ->
            emit(A.MOVE {assem = "move.l `s0,`d0"; dst = munchDataExp e0; src = munchDataExp e1})

        | T.LABEL label -> 
            emit(A.LABEL {assem = (Symbol.name label) ^ ":"; lab = label})
        
        | T.JUMP (T.NAME l, lbls) ->
            emit(A.OPER {assem = "jmp " ^ (Symbol.name l); dst = []; src = []; jump = Some lbls})

        | T.CJUMP (relop, e0, e1, t, f) ->
        begin
            match (e0, e1) with
            | (T.CONST i, e0) | (e0, T.CONST i) ->
                emit(A.OPER {assem = "cmpi.l #$" ^ Int.to_string i ^ ",`s0"; dst = []; src = [munchAddrExp e0]; jump = None});
            | _ ->
                emit(A.OPER {assem = "cmp.l `s0,`s1"; dst = []; src = [munchAddrExp e0; munchDataExp e1]; jump = None});

            (* How about cmpa !*)

            match relop with 
            | T.EQ -> 
                emit(A.OPER {assem = "beq " ^ (Symbol.name t); dst = []; src = []; jump = Some [t; f]})
            | T.NE ->
                emit(A.OPER {assem = "bne " ^ (Symbol.name t); dst = []; src = []; jump = Some [t; f]})
            | T.LT | T.ULT ->
                emit(A.OPER {assem = "blt " ^ (Symbol.name t); dst = []; src = []; jump = Some [t; f]})
            | T.GT | T.UGT ->
                emit(A.OPER {assem = "bgt " ^ (Symbol.name t); dst = []; src = []; jump = Some [t; f]})
            | T.LE | T.ULE ->
                emit(A.OPER {assem = "ble " ^ (Symbol.name t); dst = []; src = []; jump = Some [t; f]})
            | T.GE | T.UGE ->
                emit(A.OPER {assem = "bge " ^ (Symbol.name t); dst = []; src = []; jump = Some [t; f]})
        end

        | T.EXP e -> ignore(munchDataExp e)

        | stm -> 
            stm
            |> Tree.sexp_of_stm
            |> Sexp.output_hum Out_channel.stdout;
            assert(false)

    and emitCall (l, args) r = 

        let saverestore = Frame.callersaves |> List.map ~f:(fun reg ->
            let memory = Frame.exp ((Frame.allocLocal frame false), (Tree.TEMP Frame.fp)) in
                (Tree.MOVE (memory, (Tree.TEMP reg)) , Tree.MOVE ((Tree.TEMP reg), memory))
            )
        in

        let nargs = List.length args in
        
        saverestore |> List.iter ~f:(fun (s, _) -> munchStm s);

        (* We pass everything on the stack *)
        if nargs > 0 then
            emit(A.OPER {assem = "suba.l #" ^ (Int.to_string nargs) ^ ",sp"; dst = []; src = []; jump = None});

        args |> List.iteri ~f:(fun i a -> 
            emit(A.OPER {assem = "movea.l `s0,-" ^ (Int.to_string (nargs - 1 - i)) ^ "(sp)"; dst = []; src = [ munchDataExp a ]; jump = None})
        );

        emit(A.OPER {assem = "bsr `s0"; dst = []; src = [ munchAddrExp l ]; jump = None});

        if nargs > 0 then
            emit(A.OPER {assem = "adda.l #" ^ (Int.to_string nargs) ^ ",sp"; dst = []; src = []; jump = None});

        saverestore |> List.iter ~f:(fun (_, r) -> munchStm r)
    

    and munchDataExp = function

        | T.BINOP (op, e0, e1) -> 
        begin
            match op with 
            | T.PLUS ->
                begin
                    match (e0, e1) with
                    | (T.CONST i, e0) | (e0, T.CONST i) ->
                        let e0 = munchDataExp e0 in
                        data(fun r -> 
                            emit(A.MOVE {assem = "move.l `s0,`d0"; dst = r; src = e0});
                            emit(A.OPER {assem = "add.l #$" ^ (Int.to_string i) ^ ",`d0"; dst = [r]; src = [r]; jump = None}))
                    | (T.MEM e0, e1) | (e1, T.MEM e0) ->
                        let e0 = munchAddrExp e0 in
                        let e1 = munchDataExp e1 in
                        data(fun r -> 
                            emit(A.MOVE {assem = "move.l `s0,`d0"; dst = r; src = e1});
                            emit(A.OPER {assem = "add.l (`s0),`d0"; dst = [r]; src = [e0; r]; jump = None}))
                    | _ ->
                        let e0 = munchDataExp e0 in
                        let e1 = munchDataExp e1 in
                        data(fun r -> 
                            emit(A.MOVE {assem = "move.l `s0,`d0"; dst = r; src = e0});
                            emit(A.OPER {assem = "add.l `s0,`d0"; dst = [r]; src = [e1; r]; jump = None}))   
                end
            | T.MINUS ->
                let e0 = munchDataExp e0 in
                let e1 = munchDataExp e1 in
                 data(fun r -> 
                    emit(A.MOVE {assem = "move.l `s0,`d0"; dst = r; src = e0});
                    emit(A.OPER {assem = "sub.l `s0,`d0"; dst = [r]; src = [e1; r]; jump = None})) 
            | T.MUL ->
                let e0 = munchDataExp e0 in
                let e1 = munchDataExp e1 in
                 data(fun r -> 
                    emit(A.MOVE {assem = "move.l `s0,`d0"; dst = r; src = e0});
                    emit(A.OPER {assem = "mulu.w `s0,`d0"; dst = [r]; src = [e1; r]; jump = None})) 
            | T.DIV ->
                let e0 = munchDataExp e0 in
                let e1 = munchDataExp e1 in
                 data(fun r -> 
                    emit(A.MOVE {assem = "move.l `s0,`d0"; dst = r; src = e0});
                    emit(A.OPER {assem = "div.l `s0,`d0"; dst = [r]; src = [e1; r]; jump = None})) 
            | T.AND ->
                let e0 = munchDataExp e0 in
                let e1 = munchDataExp e1 in
                 data(fun r -> 
                    emit(A.MOVE {assem = "move.l `s0,`d0"; dst = r; src = e0});
                    emit(A.OPER {assem = "and.l `s0,`d0"; dst = [r]; src = [e1; r]; jump = None})) 
            | T.OR ->
                let e0 = munchDataExp e0 in
                let e1 = munchDataExp e1 in
                 data(fun r -> 
                    emit(A.MOVE {assem = "move.l `s0,`d0"; dst = r; src = e0});
                    emit(A.OPER {assem = "or.l `s0,`d0"; dst = [r]; src = [e1; r]; jump = None})) 
            | T.LSHIFT ->
                let e0 = munchDataExp e0 in
                let e1 = munchDataExp e1 in
                 data(fun r -> 
                    emit(A.MOVE {assem = "move.l `s0,`d0"; dst = r; src = e0});
                    emit(A.OPER {assem = "lsl.l `s0,`d0"; dst = [r]; src = [e1; r]; jump = None})) 
            | T.RSHIFT ->
                let e0 = munchDataExp e0 in
                let e1 = munchDataExp e1 in
                 data(fun r -> 
                    emit(A.MOVE {assem = "move.l `s0,`d0"; dst = r; src = e0});
                    emit(A.OPER {assem = "lsr.l `s0,`d0"; dst = [r]; src = [e1; r]; jump = None})) 
            | T.ARSHIFT ->
                let e0 = munchDataExp e0 in
                let e1 = munchDataExp e1 in
                 data(fun r -> 
                    emit(A.MOVE {assem = "move.l `s0,`d0"; dst = r; src = e0});
                    emit(A.OPER {assem = "asr.l `s0,`d0"; dst = [r]; src = [e1; r]; jump = None})) 
            | T.XOR ->
                let e0 = munchDataExp e0 in
                let e1 = munchDataExp e1 in
                 data(fun r -> 
                    emit(A.MOVE {assem = "move.l `s0,`d0"; dst = r; src = e0});
                    emit(A.OPER {assem = "eor.l `s0,`d0"; dst = [r]; src = [e1; r]; jump = None})) 
        end
        | T.MEM(T.BINOP(T.PLUS, T.CONST i, e0)) -> data(fun r -> emit(A.OPER {assem = "move.l " ^ Int.to_string (i / 4) ^ "(`s0),`d0"; dst = [r]; src = [munchAddrExp e0]; jump = None}))
        | T.MEM(T.BINOP(T.MINUS, T.CONST i, e0)) -> data(fun r -> emit(A.OPER {assem = "move.l " ^ Int.to_string (-i / 4) ^ "(`s0),`d0"; dst = [r]; src = [munchAddrExp e0]; jump = None}))
        | T.MEM(T.CONST i) -> data(fun r -> emit(A.OPER {assem = "move.l $"^ Int.to_string i ^ ",`d0"; dst = [r]; src = []; jump = None}))
        | T.MEM(e0) -> data(fun r -> emit(A.OPER {assem = "move.l (`s0),`d0"; dst = [r]; src = [munchAddrExp e0]; jump = None}))
        | T.TEMP t -> (t, "d")
        
        | T.NAME l -> data(fun r -> emit(A.OPER {assem = "lea.l #" ^ (Symbol.name l) ^ ",`d0" ; dst = [r]; src = []; jump = None}))
        
        | T.CONST i -> data(fun r -> emit(A.OPER {assem = "move.l #$" ^ (Int.to_string i) ^ ",`d0"; dst = [r]; src = []; jump = None}))
 
        | T.CALL (l, args) -> data(emitCall (l, args))
            
        | exp -> 
            exp
            |> Tree.sexp_of_exp
            |> Sexp.output_hum Out_channel.stdout;
            assert(false)

    and munchAddrExp = function
        
        | T.BINOP (op, e0, e1) ->
        begin
            match op with 
            | T.PLUS ->
                begin
                    match (e0, e1) with
                    | (T.CONST i, e0) | (e0, T.CONST i) ->
                        let e0 = munchDataExp e0 in
                        address(fun r -> 
                            emit(A.MOVE {assem = "movea.l `s0,`d0"; dst = r; src = e0});
                            emit(A.OPER {assem = "adda.l #$" ^ (Int.to_string i) ^ ",`d0"; dst = [r]; src = [r]; jump = None}))
                    | (T.MEM e0, e1) | (e1, T.MEM e0) ->
                        let e0 = munchAddrExp e0 in
                        let e1 = munchDataExp e1 in
                        address(fun r -> 
                            emit(A.MOVE {assem = "movea.l `s0,`d0"; dst = r; src = e1});
                            emit(A.OPER {assem = "adda.l (`s0),`d0"; dst = [r]; src = [e0; r]; jump = None}))
                    | _ ->
                        let e0 = munchDataExp e0 in
                        let e1 = munchDataExp e1 in
                        address(fun r -> 
                            emit(A.MOVE {assem = "movea.l `s0,`d0"; dst = r; src = e0});
                            emit(A.OPER {assem = "adda.l `s0,`d0"; dst = [r]; src = [e1; r]; jump = None}))                
                end
            | T.MINUS ->
                let e0 = munchDataExp e0 in
                let e1 = munchDataExp e1 in
                 address(fun r -> 
                    emit(A.MOVE {assem = "movea.l `s0,`d0"; dst = r; src = e0});
                    emit(A.OPER {assem = "suba.l `s0,`d0"; dst = [r]; src = [e1; r]; jump = None})) 
            
            | _ -> assert(false)
        end
        | T.MEM(T.BINOP(T.PLUS, T.CONST i, e0)) -> address(fun r -> emit(A.OPER {assem = "movea.l " ^ Int.to_string (i / 4) ^ "(`s0),`d0"; dst = [r]; src = [munchAddrExp e0]; jump = None}))
        | T.MEM(T.BINOP(T.MINUS, T.CONST i, e0)) -> address(fun r -> emit(A.OPER {assem = "movea.l " ^ Int.to_string (-i / 4) ^ "(`s0),`d0"; dst = [r]; src = [munchAddrExp e0]; jump = None}))
        | T.MEM(T.CONST i) -> address(fun r -> emit(A.OPER {assem = "movea.l $"^ Int.to_string i ^ ",`d0"; dst = [r]; src = []; jump = None}))
        | T.MEM(e0) -> address(fun r -> emit(A.OPER {assem = "movea.l (`s0),`d0"; dst = [r]; src = [munchAddrExp e0]; jump = None}))
        | T.TEMP t -> (t, "a")
        
        | T.NAME l -> address(fun r -> emit(A.OPER {assem = "lea.l #" ^ (Symbol.name l) ^ ",`d0" ; dst = [r]; src = []; jump = None}))
        
        | T.CONST i -> address(fun r -> emit(A.OPER {assem = "lea.l $" ^ (Int.to_string i) ^ ",`d0"; dst = [r]; src = []; jump = None}))
 
        | T.CALL (l, args) -> address(emitCall (l, args))

        | exp -> 
            exp
            |> Tree.sexp_of_exp
            |> Sexp.output_hum Out_channel.stdout;
            assert(false)

    in 
    munchStm stm;
    List.rev(!ilist)
