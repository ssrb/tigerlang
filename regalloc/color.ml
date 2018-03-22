module type T = sig
module Temp : Temp.T
module Frame : Frame.T
module Liveness : Liveness.T
type allocation = Frame.register Temp.Table.table
type color = {interference: Liveness.igraph; initial: allocation; spillCost: Graph.node -> float; registers: Frame.register list} 
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
type color = {interference: Liveness.igraph; initial: allocation; spillCost: Graph.node -> float; registers: Frame.register list} 

module Move = Comparable.Make (
    struct
        type t = Graph.node * Graph.node [@@deriving sexp, compare]
    end
)

module MS = Move.Set
module NS = Graph.Set
module NT = Graph.Table

let remove l e = List.filter ~f:((<>) e) l

let member = List.mem ~equal:(=)

let color color  = 

    let nreg = List.length color.registers in

    let gtemp = color.interference.gtemp in

    (* Machine registers, pre-assigned a color *)
    let precolored = ref NS.empty in
    
    (* Node succesfully colored *)
    let coloredNodes = ref TT.empty in

    (* Temporary registers, not precolored and not yet processed *)
    let initial = ref [] in

    (* 
        Move sets: there are 5 sets of move instructions, and every move is in exactly
        one of these sets (after "build" through the end of "main")
    *)

    (* 1 - Moves not yet ready for coalescing *)
    let activeMoves = ref MS.empty in

    (* 2 - Moves that have been colaesced *)
    let coalescedMoves = ref MS.empty in

    (* 3 - Moves whose source and target interfere *)
    let constrainedMoves = ref MS.empty in

    (* 4 - Moves that will no longer be considered for coalescing *)
    let frozenMoves = ref MS.empty in

    (* 5 - Moves enabled for possible coalescing *)
    let worklistMoves = ref MS.empty in


    (* A mapping from a node to a list moves it is associated with *)
    let moveList = ref TT.empty in

    (* The set of interference edges (u,v) in the graph. (u,v) \in G => (v,u) \in G *)
    let adjSet = ref MS.empty in
    
    (* Adjacency list representation of the graph. For each non-precolored temporary u, adjList[u] is the set of nodes that interfere with u *)
    let adjList = ref NT.empty in  
    
    (* An array containing the current degree of each node *)
    let degree = ref NT.empty in
    
    let addEdge (u, v) =

        let add (u, v) = 
            if not (MS.mem !adjSet (u, v)) then
            begin
                adjSet := MS.add !adjSet (u, v);

                if not (NS.mem !precolored u) then
                begin
                    adjList := NT.enter (!adjList, u, (NS.add (NT.look_exn (!adjList, u)) v));
                    degree := NT.enter (!degree, u, 1 + NT.look_exn (!degree, u))
                end
            end
        in 
        add (u, v);
        add (v, u)
    in

    let build () =

        let addMove (n, m) = 
            let tmp = gtemp n in
            moveList := TT.enter (!moveList, tmp, MS.add (TT.look_exn (!moveList, tmp)) m)
        in

        List.iter ~f:(fun n -> 
            let tmp = gtemp n in
            moveList := TT.enter (!moveList, tmp, MS.empty);
            adjList := NT.enter (!adjList, n, NS.empty);
            degree := NT.enter (!degree, n, 0);
            match Temp.Table.look (color.initial, tmp) with
            | Some r -> 
                (* precolored is a copy/subset of color.initial *)
                precolored := NS.add !precolored n;
                coloredNodes := TT.enter(!coloredNodes, tmp, r)
            | None ->
                initial := n::!initial
        ) color.interference.graph;

        List.iter ~f:(fun ((src, dst) as m) ->
            if not (NS.mem !precolored src) then
                addMove (src, m)
            else if not (NS.mem !precolored dst) then
                addMove (dst, m)
            else
                worklistMoves := MS.add !worklistMoves m
        ) color.interference.moves;

        List.iter ~f:(fun u ->
            List.iter ~f:(fun v ->
                addEdge (u, v)
            ) (Graph.succ u)
        ) color.interference.graph
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

    (* Registers that have been coalesced; when u <- v is coalesced, v is added to this set and u put back on some work-list (or vice versa). *)
    let coalescedNodes = ref NS.empty in

    (* Stack containing temporaries removed from the graph *)
    let selectStack = ref [] in

    let adjacent n =
        NS.diff (NT.look_exn (!adjList, n)) (NS.union (NS.of_list !selectStack) !coalescedNodes)
    in

    let enableMoves = NS.iter ~f:(fun n -> 
        MS.iter ~f:(fun mv -> 
            (* Check in activeMoves in a first place ? *)
            activeMoves := MS.remove !activeMoves mv;
            worklistMoves := MS.add !worklistMoves mv
        ) (nodeMoves n)
    )
    in

    let makeWorkList () = List.iter ~f:(fun n ->
        let d = NT.look_exn (!degree, n) in
        if d >= nreg then
            spillWorklist := n::!spillWorklist
        else if moveRelated n then
            freezeWorklist := n::!freezeWorklist
        else
            simplifyWorklist := n::!simplifyWorklist
    ) !initial
    in

    (* 
        Removing a node from the graph involves decrementing the degree of its current neighbors.
        If the degree of a neighbor is already less than K-1 then the neighbor must be move-related,
        and is not added to the simplifyWorklist. When the degree of a neighbor transitions from K
        to K-1, moves associated with its neighbors may be enabled.
    *)
    let decrementDegree n =
        (* check initAlloc ? *)
        let d = NT.look_exn (!degree, n) in
        degree := NT.enter (!degree, n, d - 1);
        if d = nreg then
        begin
            enableMoves (NS.add (adjacent n) n);
            spillWorklist := remove !spillWorklist n;
            if moveRelated n then
                freezeWorklist := n::!freezeWorklist
            else
                simplifyWorklist := n::!simplifyWorklist
        end
    in

    let simplify () =
        match !simplifyWorklist with
        | n::ns ->
            simplifyWorklist := ns;
    	    selectStack := n::!selectStack;
            NS.iter ~f:(fun m -> decrementDegree m) (adjacent n)
        | [] -> ()
    in

    let alias = ref NT.empty in

    let rec getAlias x =
        if NS.mem !coalescedNodes x then getAlias (NT.look_exn (!alias, x)) else x
    in

    let addWorkList n = 
        if not (NS.mem !precolored n)
        && not (moveRelated n)
        && NT.look_exn (!degree, n) < nreg then
        begin
            freezeWorklist := remove !freezeWorklist n;
            simplifyWorklist := n::!simplifyWorklist
        end
    in

    let combine (u, v) =
        if member !freezeWorklist v then
            freezeWorklist := remove !freezeWorklist v
        else
            spillWorklist := remove !spillWorklist v;
        coalescedNodes := NS.add !coalescedNodes v;
        alias := NT.enter (!alias, v, u);

        (* nodeMoves[u] <- nodeMoves[u] U nodeMoves[v] *)
        let mu = TT.look_exn (!moveList, (gtemp u)) in
        let mv = TT.look_exn (!moveList, (gtemp v)) in
        moveList := TT.enter (!moveList, (gtemp u), (MS.union mu mv));
        enableMoves (NS.singleton v);
        
        NS.iter ~f:(fun t ->
            addEdge (t, u);
            decrementDegree t
        ) (adjacent v);
        if NT.look_exn (!degree, u) >= nreg && member !freezeWorklist u then
        begin
            freezeWorklist := remove !freezeWorklist u;
            spillWorklist := u::!spillWorklist
        end
    in

    let coalesce () =

        let ok (t, s) = NT.look_exn (!degree, t) < nreg || NS.mem !precolored t || MS.mem !adjSet (t, s) in
        
        let conservative ns = NS.fold ~init:0 ~f:(fun cnt n -> 
            if NT.look_exn (!degree, n) >= nreg then
                cnt + 1
            else
                cnt
        ) ns < nreg 
        in

        match MS.choose !worklistMoves with
        | Some ((x, y) as m) ->
            let x = getAlias x in
            let y = getAlias y in
            let u, v = if NS.mem !precolored y then y, x else x, y in
            worklistMoves := MS.remove !worklistMoves m;
            if u = v then (
                coalescedMoves := MS.add !coalescedMoves m;
                addWorkList u
            ) else if NS.mem !precolored v || MS.mem !adjSet (u, v) then (
                constrainedMoves := MS.add !constrainedMoves m;
                addWorkList u;
                addWorkList v;
            ) else if NS.mem !precolored u
                    && NS.for_all ~f:(fun t -> ok(t, u)) (adjacent v)
                    || not (NS.mem !precolored u)
                    && conservative (NS.union (adjacent u) (adjacent v)) then (
                coalescedMoves := MS.add !coalescedMoves m;
                combine (u, v);
                addWorkList u
            ) else (
                activeMoves := MS.add !activeMoves m
            )
        | None -> ()

    in
    
    let freezeMoves u = 
        MS.iter ~f:(fun ((x, y) as m) -> 
            let v = if (getAlias u) = (getAlias y) then x else y in
            activeMoves := MS.remove !activeMoves m;
            frozenMoves := MS.add !frozenMoves m;
            if not (moveRelated v) && NT.look_exn (!degree, v) < nreg then
            begin
                freezeWorklist := remove !freezeWorklist v;
                simplifyWorklist := v::!simplifyWorklist
            end
        ) (nodeMoves u)
    in
    
    let freeze () =
        match !freezeWorklist with
        | n::ns ->
            freezeWorklist := ns;
            simplifyWorklist := n::!freezeWorklist;
            freezeMoves n
        | [] -> ()
    in
    
    let selectSpill () =
        let m = List.fold ~init:None ~f:(fun m s ->
            let c =  color.spillCost s in
            match m with 
            | Some (ms, mc) -> if c < mc then Some (ms, mc) else m
            | None -> Some (s, c)
        ) !spillWorklist
        in
        match m with
        | Some (m, _) ->
            spillWorklist := remove !spillWorklist m;
            simplifyWorklist := m::!simplifyWorklist;
            freezeMoves m
        | None -> ()
    in

    let spilledNodes = ref [] in

    let assignColors () = 
        List.iter ~f:(fun n ->

            let okColors = ref color.registers in
            NS.iter ~f:(fun w -> 
                let a = getAlias w in
                let c = TT.look (!coloredNodes, (gtemp a)) in
                if NS.mem !precolored a || Option.is_some c then
                    okColors := remove !okColors (TT.look_exn (!coloredNodes, (gtemp a)))
            ) (NT.look_exn (!adjList, n));

            let t = gtemp n in
            if List.is_empty !okColors then
                spilledNodes := t::!spilledNodes
            else
                let c = List.hd_exn !okColors in
                coloredNodes := TT.enter (!coloredNodes, t, c)

        ) !selectStack;

        NS.iter ~f:(fun n ->
            let a = getAlias n in
            let c = TT.look_exn (!coloredNodes, (gtemp a)) in
            coloredNodes := TT.enter (!coloredNodes, (gtemp n), c)
        ) !coalescedNodes
    in

    let rec main0 () = 

        if not (List.is_empty !simplifyWorklist) then
            simplify ()
        else if not (MS.is_empty !worklistMoves) then
            coalesce ()
        else if not (List.is_empty !freezeWorklist) then
            freeze ()
        else if not (List.is_empty !spillWorklist) then
            selectSpill ();
    
        if not (List.is_empty !simplifyWorklist)
        || not (MS.is_empty !worklistMoves)
        || not (List.is_empty !freezeWorklist)
        || not (List.is_empty !spillWorklist)
        then 
            main0 ()

    in

    let main () =
        build ();
        makeWorkList ();
        main0 ();
        assignColors ();
    in

    main ();

    (!coloredNodes, !spilledNodes)

end