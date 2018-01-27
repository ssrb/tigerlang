module Frame = M68kFrame
module Tree = Frame.Tree
module Temp = Frame.Temp
module Assem = Assem.F(Frame.Temp)

open Core

let codegen frame stm = 
    let ilist = ref [] in
    let emit x = ilist := x::!ilist in
    let result gen = 
        let t = Temp.newtemp() in 
        gen t; 
        t
    in
    let rec munchStm stm = ()
    and munchExp exp = ()
    in 
    munchStm stm;
    List.rev(!ilist)
