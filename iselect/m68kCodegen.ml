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
    let rec munchStm stm = ()
    and munchExp = function
        | T.MEM(T.BINOP(T.PLUS, T.CONST i, e1)) -> result(fun r -> emit(A.OPER {assem = ""; dst = [r]; src = [munchExp e1]; jump = None}))
        | _ -> assert(false)
    in 
    munchStm stm;
    List.rev(!ilist)
