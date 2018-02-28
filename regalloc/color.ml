module type T = sig
module Frame : Frame.T
module Liveness : Liveness.T
module Temp : Temp.T
type allocation = Frame.register Temp.Table.table
type color = {interference: Liveness.igraph; initial: allocation; spillCost: Graph.node -> int; registers: Frame.register list} 
val color :  color -> allocation * Temp.temp list
end

module F (Frame: Frame.T) (Liveness: Liveness.T) = struct

module Frame = Frame
module Liveness = Liveness
module Temp = Frame.Temp

type allocation = Frame.register Temp.Table.table
type color = {interference: Liveness.igraph; initial: allocation; spillCost: Graph.node -> int; registers: Frame.register list} 

open Core

let color color  = 

    let worklistMoves = Liveness.(color.interference.moves) in

    let adjList = Liveness.(color.interference.graph) in

    let adjSet = Graph.nodes adjList
        |> List.fold ~init:Graph.Table.empty ~f:(fun adjSet n -> 
            let succ = Graph.succ n |> List.fold ~init:Graph.Table.empty ~f:(fun succ n' -> Graph.Table.enter (succ, n' , ())) in
            Graph.Table.enter (adjSet, n , succ)
        ) in
    
    let degree = Graph.nodes adjList
        |> List.fold ~init:Graph.Table.empty ~f:(fun degree n -> Graph.Table.enter (degree, n , ref (List.length (Graph.succ n)))) in

    let precolored = [] in
    let initial = [] in
    let simplifyWorklist = [] in
    let freezeWorklist = []in
    let spillWorklist = [] in
    let spilledNodes = [] in
    let coalescedNodes = [] in
    let coloredNodes = [] in
    let selectStack = [] in
    let coalescedMoves = [] in
    let constrainedMoves = [] in
    let frozenMoves = [] in
    let activeMoves = [] in
    let moveList = () in
    let alias = () in
    let color = () in
    
    (Temp.Table.empty, [])
end