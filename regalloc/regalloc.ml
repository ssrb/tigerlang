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

let member = List.mem ~equal:(=) 

let rec alloc (asm, frame) = 

    let rewriteProgram (asm, frame, spills) = 

        let rewriteProgram' asm (spill : Temp.temp) = 

            let memory = Frame.exp ((Frame.allocLocal frame true), (Tree.TEMP Frame.fp)) in
            let fetch t = Codegen.codegen frame (Tree.MOVE (memory, Tree.TEMP(t))) in
            let store t = Codegen.codegen frame (Tree.MOVE (Tree.TEMP(t), memory)) in

            let rewriteOperands fs ops  = 
                if member ops spill then
                    let t = Temp.newtemp () in
                    (fs t, List.map ~f:(fun o -> if o = spill then t else o) ops)
                else
                    ([], ops)
            in

            let rewriteInstruction instr =
                match instr with
                | A.OPER {assem; dst; src; jump} ->
                    let (fetch, src') = rewriteOperands fetch src in
                    let (store, dst') = rewriteOperands store dst in
                    fetch @ A.OPER {assem; dst = dst'; src = src'; jump}::store
                | A.MOVE {assem; dst; src} ->
                    let (fetch, [ src' ]) = rewriteOperands fetch [ src ] in
                    let (store, [ dst' ]) = rewriteOperands store [ dst ] in
                    fetch @ A.MOVE {assem; dst = dst'; src = src'}::store
                | _ -> [ instr ]
            in

            List.fold ~init:[] ~f:(fun asm instr -> (rewriteInstruction instr) @ asm) (List.rev asm)
        in

        List.fold ~init:asm ~f:rewriteProgram' spills
    in

    let fgraph, _ginstr =  Makegraph.instrs2graph asm in
    let igraph = Liveness.interferenceGraph fgraph in

    let spillCost n =
        let tmp = igraph.gtemp n in
        let used t l = if member l t then 1 else 0 in
        let usecnt = List.fold ~init:0 ~f:(fun usecnt instr ->
            match instr with
            | A.OPER {assem; dst; src; jump} ->
                usecnt + (used tmp dst) + (used tmp src)
            | A.MOVE {assem; dst; src} ->
                usecnt + (used tmp [ dst ]) + (used tmp [ src ])
            | _ -> usecnt
        ) asm
        in
        (float_of_int usecnt) /. (float_of_int(List.length (Graph.adj n)))
    in

	let colors, spills = Color.color {interference = igraph; initial = Temp.Table.empty; spillCost; registers = Frame.registers} in
    match spills with
    | [] ->
        
        let asm = List.filter ~f:(fun instr ->
            match instr with
            | A.MOVE {assem; dst; src} ->
            begin
                match TT.look (colors, dst), TT.look (colors, src) with
                | Some dst, Some src -> dst <> src
                | _ -> true
            end
            | _ -> true
        ) asm
        in

        (asm, colors)

    | _ -> 
        let asm = rewriteProgram (asm, frame, spills) in
        alloc (asm, frame)
end