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

module Move = Comparable.Make(
    struct
        type t = Graph.node * Graph.node [@@deriving sexp, compare]
    end
)

module MS = Move.Set
module NS = Graph.Set
module NT = Graph.Table

let remove l e = List.filter ~f:((<>) e) l


let color color  = 

    let nreg = List.length color.registers in

    let gtemp = color.interference.gtemp in

    (* Machine registers, pre-assigned a color *)
    let precolored = ref NS.empty in
    
    (* Node succesfully colored *)
    let coloredNodes = ref TT.empty in

    (* Temporary registers, not precolored and not yet processed *)
    let initial = ref [] in

    (* Moves enabled for possible coalescing *)
    let worklistMoves = ref MS.empty in

    (* Moves not yet ready for coalescing *)
    let activeMoves = ref MS.empty in

    (* A mapping from a node to a list moves it is associated with *)
    let moveList = ref TT.empty in

    let build () =

        let addMove (n, m) = 
            let tmp = gtemp n in
            let moves = match TT.look (!moveList, tmp) with
            | Some moves -> moves
            | None -> MS.empty
            in
            moveList := TT.enter (!moveList, tmp, (MS.add moves m))
        in

        begin
            List.iter ~f:(fun n -> 
                let tmp = gtemp n in
                match Temp.Table.look (color.initial, tmp) with
                | Some r -> 
                    (* precolored is a copy/subset of color.initial *)
                    precolored := NS.add !precolored n;
                    coloredNodes := TT.enter(!coloredNodes, tmp, r);
                | None ->
                    initial := n::!initial
            ) color.interference.graph;

            List.iter color.interference.moves ~f:(fun ((src, dst) as m) ->
                if not (NS.mem !precolored src) then
                    addMove (src, m)
                else if not (NS.mem !precolored dst) then
                    addMove (dst, m)
                else
                    worklistMoves := MS.add !worklistMoves m
            )
        end
    in

    let nodeMoves n =
        MS.inter (TT.look_exn (!moveList, (gtemp n))) (MS.union !activeMoves !worklistMoves)
    in

    let moveRelated n =
        not (MS.is_empty (nodeMoves n))
    in

    (* High-degree nodes *)
    let spillWorklist = ref [] in

    (* Low-degree move related nodes *)
    let freezeWorklist = ref [] in

    (* List of low-degree non-move-related nodes*)
    let simplifyWorklist = ref [] in

    (* Adjacency list representation of the graph. For each non-precolored temporary u, adjList[u] is the set of nodes that interfere with u *)
    (* let adjList = color.interference.graph in *)

    (* The set of interference edges (u,v) in the graph. (u,v) \in G => (v,u) \in G *)
    let adjList = color.interference.graph |> List.fold ~init:NT.empty ~f:(fun adjSet n -> 
        let succ = Graph.adj n |> List.fold ~init:NS.empty ~f:(fun succ n' -> NS.add succ n') in
        NT.enter (adjSet, n , succ)
    ) 
    in

    (* Registers that have been coalesced; when u <- v is coalesced, v is added to this set and u put back on some work-list (or vice versa). *)
    let coalescedNodes = ref NS.empty in

    (* Stack containing temporaries removed from the graph *)
    let selectStack = ref [] in

    let adjacent n =
        NS.diff (NT.look_exn (adjList, n)) (NS.union (NS.of_list !selectStack) !coalescedNodes)
    in

    (* An array containing the current degree of each node *)
    let degree = color.interference.graph |> List.fold ~init:NT.empty ~f:(fun degree n -> NT.enter (degree, n , ref (List.length (Graph.succ n)))) in

    let decrementDegree n =
        let enableMoves ns = () in
        let d = NT.look_exn (degree, n) in
        d := !d - 1;
        if !d = nreg then
        begin
            enableMoves (NS.add (adjacent n) n);
            spillWorklist := remove !spillWorklist n;
            if moveRelated n then
                freezeWorklist := n::!freezeWorklist
            else
                simplifyWorklist := n::!simplifyWorklist
        end
    in

    let makeWorkList () =
        List.iter ~f:(fun n ->
            let d = NT.look_exn (degree, n) in
            if !d >= nreg then
                spillWorklist := n::!spillWorklist
            else if moveRelated n then
                freezeWorklist := n::!freezeWorklist
            else
                simplifyWorklist := n::!simplifyWorklist
        ) !initial
    in

    let simplify () =
        match !simplifyWorklist with
        | n::ns ->
            simplifyWorklist := ns;
    	    selectStack := n::!selectStack;
            NS.iter ~f:(fun m -> decrementDegree m) (adjacent n)
        | _ -> ()
    in

    let spilledNodes = [] in
    let coalescedMoves = [] in
    let constrainedMoves = [] in
    let frozenMoves = [] in
    

    let alias = () in
    let color = () in
    
    ((Temp.Table.empty : allocation), ([] : Temp.temp list))
end