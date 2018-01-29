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
        
        | T.MOVE (T.MEM (T.BINOP (T.PLUS, e0, (T.CONST i))), T.MEM (T.BINOP (T.PLUS, e1, (T.CONST j))))
        | T.MOVE (T.MEM (T.BINOP (T.PLUS, (T.CONST i), e0)), T.MEM (T.BINOP (T.PLUS, e1, (T.CONST j))))
        | T.MOVE (T.MEM (T.BINOP (T.PLUS, e0, (T.CONST i))), T.MEM (T.BINOP (T.PLUS, (T.CONST j), e1)))
        | T.MOVE (T.MEM (T.BINOP (T.PLUS, (T.CONST i), e0)), T.MEM (T.BINOP (T.PLUS, (T.CONST j), e1)))
         -> emit(A.OPER {assem = "move.l " ^ Int.to_string (j / 4) ^ "(s1)," ^ Int.to_string (i / 4) ^ "(s0)"; dst = []; src = [munchExp e0; munchExp e1]; jump = None})

        | T.MOVE (T.MEM (T.BINOP (T.PLUS, e0, (T.CONST i))), e1)
        | T.MOVE (T.MEM (T.BINOP (T.PLUS, (T.CONST i), e0)), e1)
         -> emit(A.OPER {assem = "move.l s1," ^ Int.to_string (i / 4) ^ "(s0)"; dst = []; src = [munchExp e0; munchExp e1]; jump = None})

        | T.MOVE (e0, T.MEM (T.BINOP (T.PLUS, e1, (T.CONST i))))
        | T.MOVE (e0, T.MEM (T.BINOP (T.PLUS, (T.CONST i), e1)))
         -> emit(A.OPER {assem = "move.l " ^ Int.to_string (i / 4) ^ "(s1),s0"; dst = []; src = [munchExp e0; munchExp e1]; jump = None})

        | T.MOVE (T.MEM (T.BINOP (T.MINUS, e0, (T.CONST i))), e1)
        | T.MOVE (T.MEM (T.BINOP (T.MINUS, (T.CONST i), e0)), e1)
         -> emit(A.OPER {assem = "move.l s1," ^ Int.to_string (-i / 4) ^ "(s0)"; dst = []; src = [munchExp e0; munchExp e1]; jump = None})

        | T.MOVE (e0, T.MEM (T.BINOP (T.MINUS, e1, (T.CONST i))))
        | T.MOVE (e0, T.MEM (T.BINOP (T.MINUS, (T.CONST i), e1)))
         -> emit(A.OPER {assem = "move.l " ^ Int.to_string (-i / 4) ^ "(s1),s0"; dst = []; src = [munchExp e0; munchExp e1]; jump = None})

        | T.MOVE(e1, e2) -> emit(A.OPER {assem = "move.l"; dst = [munchExp e1]; src = [munchExp e2]; jump = None})
        | _ -> assert(false)

    and munchExp = function
        | T.MEM(T.BINOP(T.PLUS, T.CONST i, e1)) -> result(fun r -> emit(A.OPER {assem = ""; dst = [r]; src = [munchExp e1]; jump = None}))
        | _ -> assert(false)
    in 
    munchStm stm;
    List.rev(!ilist)
