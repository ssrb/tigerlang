module F = functor(Codegen : Codegen.T) ->
struct
module Frame = Codegen.Frame
module Assem = Codegen.Assem

module Makegraph = Makegraph.F(Assem)
module Flowgraph = Makegraph.Flow
module Liveness = Liveness.F(Flowgraph)
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

            let rewriteInstruction = function
                | A.OPER op ->
                    let (fetch, src') = rewriteOperands fetch op.src in
                    let (store, dst') = rewriteOperands store op.dst in
                    fetch @ A.OPER {op with dst = dst'; src = src'}::store
                | A.MOVE mv ->
                    let (fetch, [ src' ]) = rewriteOperands fetch [ mv.src ] in
                    let (store, [ dst' ]) = rewriteOperands store [ mv.dst ] in
                    fetch @ A.MOVE {mv with dst = dst'; src = src'}::store
                | instr -> [ instr ]
            in

            List.fold ~init:[] ~f:(fun asm instr -> (rewriteInstruction instr) @ asm) (List.rev asm)
        in

        List.fold ~init:asm ~f:rewriteProgram' spills
    in

    let fgraph, ginstr = Makegraph.instrs2graph asm in

    Flowgraph.show Out_channel.stdout fgraph (fun n -> n |> ginstr |> Assem.format Temp.makestring);

    let igraph = Liveness.interferenceGraph fgraph in

    let spillCost n =
        let tmp = igraph.gtemp n in
        let used t l = if member l t then 1 else 0 in
        let usecnt = List.fold ~init:0 ~f:(fun usecnt -> function
            | A.OPER op ->
                usecnt + (used tmp op.dst) + (used tmp op.src)
            | A.MOVE mv ->
                usecnt + (used tmp [ mv.dst ]) + (used tmp [ mv.src ])
            | _ -> usecnt
        ) asm
        in
        (float_of_int usecnt) /. (float_of_int(List.length (Graph.adj n)))
    in

	let colors, spills = Color.color {interference = igraph; initial = Frame.tempMap; spillCost; registers = Frame.registers} in
    match spills with
    | [] ->
        
        Liveness.show Out_channel.stdout igraph ~color:(fun n -> 
            match TT.look (colors, igraph.gtemp n) with
            | Some c ->
            begin
                match List.findi Frame.registers ~f:(fun _ e -> c = e)  with
                | Some (i, _) -> 
                    List.nth_exn ["Aquamarine";
                    "Blue";
                    "\"Slate Blue\"";
                    "\"Sky Blue\"";
                    "\"Steel Blue\"";
                    "Coral";
                    "Cyan";
                    "Goldenrod";
                    "Gray";
                    "\"Slate Gray\"";
                    "Green";
                    "\"Olive Green\"";
                    "\"Sea Green\"";
                    "\"Spring Green\"";
                    "Khaki";
                    "Magenta";
                    "Orange";
                    "Orchid";
                    "Pink";
                    "Purple";
                    "\"Violet Red\"";
                    "Red";
                    "Salmon";
                    "Turquoise";
                    "Violet";
                    "Yellow"] i
                | None -> "black"
            end
            | None -> "black"
        );

        let asm = List.filter ~f:(function
            | A.MOVE mv ->
            begin
                match TT.look (colors, mv.dst), TT.look (colors, mv.src) with
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