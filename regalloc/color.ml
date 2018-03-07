module type T = sig
module Temp : Temp.T
module Frame : Frame.T
module Liveness : Liveness.T
type allocation = Frame.register Temp.Table.table
type color = {interference: Liveness.igraph; initial: allocation; spillCost: Graph.node -> int; registers: Frame.register list} 
val color :  color -> allocation * Temp.temp list
end

module F (Frame: Frame.T) (Liveness: Liveness.T with module Temp = Frame.Temp) = struct

open Core
open Liveness

module Frame = Frame
module Liveness = Liveness
module Temp = Frame.Temp
module TT = Temp.Table

type allocation = Frame.register Temp.Table.table
type color = {interference: Liveness.igraph; initial: allocation; spillCost: Graph.node -> int; registers: Frame.register list} 

let color color  = 

    (* machine registers, pre-assigned a color *)
    let precolored = ref TT.empty in
    (* Node succesfully colored *)
    let coloredNodes = ref (Set.empty ~comparator:Graph.Comp.comparator) in
    (* Temporary registers, not precolored and not yet processed *)
    let initial = ref [] in

    List.iter ~f:(fun n -> 
      let tmp = color.interference.gtemp n in
      match Temp.Table.look (color.initial, tmp) with
      | Some r -> 
        (* precolored is a copy/subset of color.initial *)
        precolored := TT.enter(!precolored, tmp, r);
        coloredNodes := Set.add !coloredNodes n;
      | None ->
        initial := n::!initial
    ) color.interference.graph;

    (* moves enabled for possible coalescing *)
    let worklistMoves = color.interference.moves in
    (* a mapping from a node to a list moves it is associated with *)
    let moveList = () in

    let adjList = color.interference.graph in

    let adjSet = adjList |> List.fold ~init:Graph.Table.empty ~f:(fun adjSet n -> 
            let succ = Graph.succ n |> List.fold ~init:Graph.Table.empty ~f:(fun succ n' -> Graph.Table.enter (succ, n' , ())) in
            Graph.Table.enter (adjSet, n , succ)
        ) in
    
    let degree = adjList |> List.fold ~init:Graph.Table.empty ~f:(fun degree n -> Graph.Table.enter (degree, n , ref (List.length (Graph.succ n)))) in

    

  
    let spillWorklist = [] in
    let freezeWorklist = [] in
    let simplifyWorklist = [] in
    let spilledNodes = [] in
    let coalescedNodes = [] in
    let selectStack = [] in
    let coalescedMoves = [] in
    let constrainedMoves = [] in
    let frozenMoves = [] in
    let activeMoves = [] in

    let alias = () in
    let color = () in
    
    ((Temp.Table.empty : allocation), ([] : Temp.temp list))
end