module F = functor(Codegen : Codegen.T) ->
struct
module Frame = Codegen.Frame
module Assem = Codegen.Assem

module Makegraph = Makegraph.F(Assem)
module Liveness = Liveness.F(Makegraph.Flow)
module Color = Color.F (Frame) (Liveness)
module A = Assem

module Temp = Assem.Temp
module TT = Temp.Table
module Tree = Frame.Tree

open Core
open Color

let rec alloc (asm, frame) = 

    let rewriteProgram (asm, frame, spills) = 
        let rewriteProgram' asm (spill : Temp.temp) = 
            let ae = Frame.exp ((Frame.allocLocal frame true), (Tree.TEMP Frame.fp)) in
            let fetch t = Codegen.codegen frame (Tree.MOVE (ae, Tree.TEMP(t))) in
            let store t = Codegen.codegen frame (Tree.MOVE (Tree.TEMP(t), ae)) in
            let rewriteOperands fs args  = 
                if List.mem ~equal:(=) args spill then
                    let t = Temp.newtemp () in
                    (fs t, List.map ~f:(fun a -> if a = spill then t else a) args)
                else
                    ([], args)
            in
            let rewriteInstruction asm instr =
                match instr with
                | A.OPER {assem; dst; src; jump} ->
                    let (fetch, src') = rewriteOperands fetch src in
                    let (store, dst') = rewriteOperands store dst in
                    fetch @ [ A.OPER {assem; dst = dst'; src = src'; jump} ] @ store @ asm
                | A.MOVE {assem; dst; src} ->
                    let (fetch, [ src' ]) = rewriteOperands fetch [ src ] in
                    let (store, [ dst' ]) = rewriteOperands store [ dst ] in
                    fetch @ [ A.MOVE {assem; dst = dst'; src = src'} ] @ store @ asm
                | _ -> instr::asm
            in
            List.fold ~init:[] ~f:rewriteInstruction (List.rev asm)
        in
        List.fold ~init:asm ~f:rewriteProgram' spills
    in

    let fgraph =  Makegraph.instrs2graph asm in
    let igraph = Liveness.interferenceGraph fgraph in
	let colors, spills = Color.color {interference = igraph; initial = Temp.Table.empty; spillCost = (fun _ -> 0); registers = Frame.registers} in
    match spills with
    | [] ->
        
        let asm = List.filter ~f:(fun instr ->
            match instr with
            | A.MOVE {assem; dst; src} ->
                let dst = TT.look_exn (colors, dst) in
                let src = TT.look_exn (colors, src) in
                dst <> src
            | _ -> true
        ) asm
        in

        (asm, colors)

    | _ -> 
        let asm = rewriteProgram (asm, frame, spills) in
        alloc (asm, frame)
end