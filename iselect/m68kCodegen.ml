module Frame = M68kFrame
module Tree = Frame.Tree
module Temp = Frame.Temp
module Assem = Assem.F(Frame.Temp)

open Core

let codegen frame stm =
    let module T = Tree in
    let module A = Assem in
    let ilist = ref [] in
    let emit x = ilist := x::!ilist in
    let result gen = 
        let t = Temp.newtemp() in 
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
                    emit(A.MOVE {assem = "move.l " ^ Int.to_string (j / 4) ^ "(s0)," ^ Int.to_string (i / 4) ^ "(d0)"; dst = munchAddrExp e0; src = munchAddrExp e1})
                else
                    emit(A.OPER {assem = "move.l " ^ Int.to_string (j / 4) ^ "(s1)," ^ Int.to_string (i / 4) ^ "(s0)"; dst = []; src = [munchAddrExp e0; munchAddrExp e1]; jump = None})
  
            | T.MEM (T.BINOP (T.MINUS, e1, (T.CONST j)))
            | T.MEM (T.BINOP (T.MINUS, (T.CONST j), e1)) ->
                emit(A.OPER {assem = "move.l " ^ Int.to_string (-j / 4) ^ "(s1)," ^ Int.to_string (i / 4) ^ "(s0)"; dst = []; src = [munchAddrExp e0; munchAddrExp e1]; jump = None})

            | T.MEM (T.CONST i) ->
                emit(A.OPER {assem = "move.l $" ^ Int.to_string (i / 4) ^ ",(s0)"; dst = []; src = [munchAddrExp e0]; jump = None})

            | T.MEM e1 ->
                emit(A.OPER {assem = "move.l (s1)," ^ Int.to_string (i / 4) ^ "(s0)"; dst = []; src = [munchAddrExp e0; munchAddrExp e1]; jump = None})

            | T.CONST i ->
                emit(A.OPER {assem = "move.l #$" ^ Int.to_string (i / 4) ^ ",(s0)"; dst = []; src = [munchAddrExp e0]; jump = None})

            | e1 ->
                emit(A.OPER {assem = "move.l s1," ^ Int.to_string (i / 4) ^ "(s0)"; dst = []; src = [munchAddrExp e0; munchDataExp e1]; jump = None})
        end 

        | T.MOVE (T.MEM (T.BINOP (T.MINUS, e0, (T.CONST i))), e1)
        | T.MOVE (T.MEM (T.BINOP (T.MINUS, (T.CONST i), e0)), e1) ->
        begin
            match e1 with
            | T.MEM (T.BINOP (T.PLUS, e1, (T.CONST j)))
            | T.MEM (T.BINOP (T.PLUS, (T.CONST j), e1)) ->
                emit(A.OPER {assem = "move.l " ^ Int.to_string (j / 4) ^ "(s1)," ^ Int.to_string (-i / 4) ^ "(s0)"; dst = []; src = [munchAddrExp e0; munchAddrExp e1]; jump = None})
  
            | T.MEM (T.BINOP (T.MINUS, e1, (T.CONST j)))
            | T.MEM (T.BINOP (T.MINUS, (T.CONST j), e1)) ->
                if i = j then
                    emit(A.MOVE {assem = "move.l " ^ Int.to_string (-j / 4) ^ "(s0)," ^ Int.to_string (-i / 4) ^ "(d0)"; dst = munchAddrExp e0; src = munchAddrExp e1})
                else
                    emit(A.OPER {assem = "move.l " ^ Int.to_string (-j / 4) ^ "(s1)," ^ Int.to_string (-i / 4) ^ "(s0)"; dst = []; src = [munchAddrExp e0; munchAddrExp e1]; jump = None})

            | T.MEM (T.CONST i) ->
                emit(A.OPER {assem = "move.l $" ^ Int.to_string (i / 4) ^ ",(s0)"; dst = []; src = [munchAddrExp e0]; jump = None})

            | T.MEM e1 ->
                emit(A.OPER {assem = "move.l (s1)," ^ Int.to_string (-i / 4) ^ "(s0)"; dst = []; src = [munchAddrExp e0; munchAddrExp e1]; jump = None})

            | T.CONST i ->
                emit(A.OPER {assem = "move.l #$" ^ Int.to_string (i / 4) ^ ",(s0)"; dst = []; src = [munchAddrExp e0]; jump = None})

            | e1 ->
                emit(A.OPER {assem = "move.l s1," ^ Int.to_string (-i / 4) ^ "(s0)"; dst = []; src = [munchAddrExp e0; munchDataExp e1]; jump = None})
        end

        | T.MOVE (T.MEM e0, e1) ->
        begin
            match e1 with
            | T.MEM (T.BINOP (T.PLUS, e1, (T.CONST j)))
            | T.MEM (T.BINOP (T.PLUS, (T.CONST j), e1)) ->
                emit(A.OPER {assem = "move.l " ^ Int.to_string (j / 4) ^ "(s1),(s0)"; dst = []; src = [munchAddrExp e0; munchAddrExp e1]; jump = None})
  
            | T.MEM (T.BINOP (T.MINUS, e1, (T.CONST j)))
            | T.MEM (T.BINOP (T.MINUS, (T.CONST j), e1)) ->
                emit(A.OPER {assem = "move.l " ^ Int.to_string (-j / 4) ^ "(s1),(s0)"; dst = []; src = [munchAddrExp e0; munchAddrExp e1]; jump = None})

            | T.MEM (T.CONST i) ->
                emit(A.OPER {assem = "move.l $" ^ Int.to_string (i / 4) ^ ",(s0)"; dst = []; src = [munchAddrExp e0]; jump = None})

            | T.MEM e1 ->
                if e0 <> e1 then
                    emit(A.OPER {assem = "move.l (s1),(s0)"; dst = []; src = [munchAddrExp e0; munchAddrExp e1]; jump = None});

            | T.CONST i ->
                emit(A.OPER {assem = "move.l #$" ^ Int.to_string (i / 4) ^ ",(s0)"; dst = []; src = [munchAddrExp e0]; jump = None})

            | e1 ->
                emit(A.OPER {assem = "move.l s1,(s0)"; dst = []; src = [munchAddrExp e0; munchDataExp e1]; jump = None})
        end 

        | T.MOVE(e0, e1) -> 
            emit(A.MOVE {assem = "move.l s0,d0"; dst = munchDataExp e0; src = munchDataExp e1})

        | T.LABEL label -> 
            emit(A.LABEL {assem = (Symbol.name label) ^ ":"; lab = label})
        
        | JUMP (e0, lbls) ->
            emit(A.OPER {assem = ""; dst = []; src = []; jump = None})
        (*| CJUMP (relop, e0, e1, t, f*)
        (*| EXP of exp*)

        | _ -> assert(false)

    and munchDataExp = function
        | T.MEM(T.BINOP(T.PLUS, T.CONST i, e0)) -> result(fun r -> emit(A.OPER {assem = "move.l " ^ Int.to_string (i / 4) ^ "(s0),d0"; dst = [r]; src = [munchAddrExp e0]; jump = None}))
        | T.MEM(T.BINOP(T.MINUS, T.CONST i, e0)) -> result(fun r -> emit(A.OPER {assem = "move.l " ^ Int.to_string (-i / 4) ^ "(s0),d0"; dst = [r]; src = [munchAddrExp e0]; jump = None}))
        | T.MEM(T.CONST i) -> result(fun r -> emit(A.OPER {assem = "move.l $"^ Int.to_string i ^ ",d0"; dst = [r]; src = []; jump = None}))
        | T.MEM(e0) -> result(fun r -> emit(A.OPER {assem = "move.l (s0),d0"; dst = [r]; src = [munchAddrExp e0]; jump = None}))
        | T.CONST i -> result(fun r -> emit(A.OPER {assem = "move.l #$" ^ Int.to_string i ^ ",d0"; dst = [r]; src = []; jump = None}))        
        | _ -> assert(false)
    and munchAddrExp = function
        | _ -> assert(false)
    in 
    munchStm stm;
    List.rev(!ilist)
