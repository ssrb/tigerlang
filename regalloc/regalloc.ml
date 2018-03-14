module F = functor(Frame : Frame.T) ->
struct
module Temp = Frame.Temp
module Assem = Frame.Assem

module Makegraph = Makegraph.F(Assem)
module Liveness = Liveness.F(Makegraph.Flow)
module Color = Color.F (Frame) (Liveness)
module A = Assem

open Core
open Color

let rec alloc (asm, (frame : Frame.frame)) = 

    let rewriteProgram (asm, frame, spills) = 
        asm 
    in

    let fgraph =  Makegraph.instrs2graph asm in
    let igraph = Liveness.interferenceGraph fgraph in
	let colors, spills = Color.color {interference = igraph; initial = Temp.Table.empty; spillCost = (fun _ -> 0); registers = Frame.registers} in
    match spills with
    | [] ->
        let asm = List.filter ~f:(fun instr ->
            match instr with
            | A.MOVE {assem; dst; src} ->
                let dst = Temp.Table.look_exn (colors, dst) in
                let src = Temp.Table.look_exn (colors, src) in
                dst <> src
            | _ -> true
        ) asm
        in
        (asm, colors)

    | _ -> 
        let asm = rewriteProgram (asm, frame, spills) in
        alloc (asm, frame)
end