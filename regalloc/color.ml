module type T = sig
module Temp : Temp.T
module Frame : Frame.T
module Liveness : Liveness.T
type allocation = Frame.register Temp.Table.table
type color = {interference: Liveness.igraph; initial: allocation; spillCost: Graph.node -> float; targetmodel: Frame.targetmodel} 
val color :  color -> allocation * Temp.temp list
end

module F (Frame: Frame.T) (Liveness: Liveness.T with module Assem = Frame.Assem) = struct

open Core
open Liveness

module Frame = Frame
module Liveness = Liveness
module Temp = Frame.Temp
module TT = Temp.Table

type allocation = Frame.register Temp.Table.table
type color = {interference: Liveness.igraph; initial: allocation; spillCost: Graph.node -> float; targetmodel: Frame.targetmodel} 

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

    (* let k = List.length color.targetmodel.regs in *)

    let gtemp = color.interference.gtemp in

    let iscolorable = color.targetmodel.colorable in

    let colorable = ref NT.empty in

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
    (* let degree = ref NT.empty in *)
    
    let addEdge (u, v) =

        let add (u, v) = 
            if not (MS.mem !adjSet (u, v)) then
            begin
                adjSet := MS.add !adjSet (u, v);

                if not (NS.mem !precolored u) then
                begin
                    adjList := NT.enter (!adjList, u, (NS.add (NT.look_exn (!adjList, u)) v));
                    (* degree := NT.enter (!degree, u, 1 + NT.look_exn (!degree, u)) *)
                end
            end
        in 
        add (u, v);
        add (v, u)
    in

    let build () =

        let addMove (n, m) = 
            let tmp = (gtemp n).temp in
            moveList := TT.enter (!moveList, tmp, MS.add (TT.look_exn (!moveList, tmp)) m)
        in

        color.interference.graph |> List.iter ~f:(fun n -> 
            let tmp = (gtemp n).temp in

            moveList := TT.enter (!moveList, tmp, MS.empty);
            adjList := NT.enter (!adjList, n, NS.empty);
            (* degree := NT.enter (!degree, n, 0); *)
            
            match TT.look (color.initial, tmp) with
            | Some r ->
                precolored := NS.add !precolored n;
                coloredNodes := TT.enter(!coloredNodes, tmp, r)
            | None ->
                initial := n::!initial
        );

        color.interference.moves |> List.iter ~f:(fun ((src, dst) as m) ->
            if not (NS.mem !precolored src) then
                addMove (src, m);
            if not (NS.mem !precolored dst) then
                addMove (dst, m);
            worklistMoves := MS.add !worklistMoves m
        );

        color.interference.graph |> List.iter ~f:(fun u ->
            List.iter ~f:(fun v ->
                addEdge (u, v)
            ) (Graph.succ u)
        )

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

    let nodeMoves n =
        MS.inter (TT.look_exn (!moveList, (gtemp n).temp)) (MS.union !activeMoves !worklistMoves)
    in

    let moveRelated n =
        not (MS.is_empty (nodeMoves n))
    in

    let makeWorkList () = List.iter ~f:(fun n ->

        let c = iscolorable (gtemp n) (adjacent n |> NS.to_list |> List.map ~f:gtemp) in
        
        colorable := NT.enter (!colorable, n, c);

        if c then
            simplifyWorklist := n::!simplifyWorklist
        else
            spillWorklist := n::!spillWorklist
            
    ) !initial
    in

    let addWorkList n = 
        if not (NS.mem !precolored n)
        && not (moveRelated n)
        && iscolorable (gtemp n) (adjacent n |> NS.to_list |> List.map ~f:gtemp) then
        begin
            freezeWorklist := remove !freezeWorklist n;
            simplifyWorklist := n::!simplifyWorklist
        end
    in

    let enableMoves = NS.iter ~f:(fun n -> 
        MS.iter ~f:(fun mv -> 
            if MS.mem !activeMoves mv then
            begin
                activeMoves := MS.remove !activeMoves mv;
                worklistMoves := MS.add !worklistMoves mv
            end
        ) (nodeMoves n)
    )
    in

    (* 
        Removing a node from the graph involves decrementing the degree of its current neighbors.
        If the degree of a neighbor is already less than K-1 then the neighbor must be move-related,
        and is not added to the simplifyWorklist. When the degree of a neighbor transitions from K
        to K-1, moves associated with its neighbors may be enabled.
    *)
    let decrementDegree n =
        (* check initAlloc ? *)
        (* let d = NT.look_exn (!degree, n) in
        degree := NT.enter (!degree, n, d - 1); *)

        let cold = NT.look_exn (!colorable, n) in
        let c = iscolorable (gtemp n) (adjacent n |> NS.to_list |> List.map ~f:gtemp) in

        if (not cold) && c then
        begin
            (* 
                We enable move associated with
                - n itself as Briggs strategy might now apply; ie coalescing might lead to an insignificant degree node 
                    thus not changing the colorability of the graph;
                - neighbors of n as George strategy might now apply too; ie combination of insignificant degree neighbor or
                    shared interfering neighbors
                
                Enabling too many moves is "harmless" to the result as long as we always check that coalescing is conservative.
                It will have a performance impact though.
            *)
            colorable := NT.enter (!colorable, n, c); 
            enableMoves (NS.add (adjacent n) n);
            spillWorklist := remove !spillWorklist n;
            if moveRelated n then
                freezeWorklist := n::!freezeWorklist
            else
                simplifyWorklist := n::!simplifyWorklist
        end;
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

    let combine (u, v) =

        (* 
            At the begining of the coalesce phase, "simplifyWorklist" is empty.
            "v" is either in "freezeWorklist" or "spillWorklist"
        *)
        if member !freezeWorklist v then
            freezeWorklist := remove !freezeWorklist v
        else
            spillWorklist := remove !spillWorklist v;

        coalescedNodes := NS.add !coalescedNodes v;
        alias := NT.enter (!alias, v, u);

        (* nodeMoves[u] <- nodeMoves[u] U nodeMoves[v] *)
        let tu = (gtemp u).temp in
        let tv = (gtemp v).temp in
        let mu = TT.look_exn (!moveList, tu) in
        let mv = TT.look_exn (!moveList, tv) in
        moveList := TT.enter (!moveList, tu, (MS.union mu mv));
        enableMoves (NS.singleton v);
        
        NS.iter ~f:(fun t ->
            (* "addEdge" increments degrees of both "t" and "u" *)
            addEdge (t, u);
            (* but degree of "t" must stay unchanged as we "remove" "v" *)
            decrementDegree t
        ) (adjacent v);

        (* 
            Again, at the begining of the coalesce phase, "simplifyWorklist" is empty.
            We might only add neighbor of "v" into it but not "u".
            So that we only check if "u" is in "freezeWorklist"
         *)
        if not (iscolorable (gtemp u) (adjacent u |> NS.to_list |> List.map ~f:gtemp))
            && member !freezeWorklist u then
        begin
            freezeWorklist := remove !freezeWorklist u;
            spillWorklist := u::!spillWorklist
        end
    in

    (*
        Only moves in the worklist are considered in the coalesce phase.
        When a node is coalesced, it may no longer be move-related and can be
        added to the "simplifyWorklist" by the procedurer "addWorklist". 
        "ok" implements the heuristic used for coalescing a precolored register.
        "conservative" implements the conservative coalescing heuristic.
    *)
    let coalesce () =

        (* George strategy for a precolored node: we check that colorability won't change *)
        let ok (t, s) = (iscolorable (gtemp t) (adjacent t |> NS.to_list |> List.map ~f:gtemp)) || NS.mem !precolored t || MS.mem !adjSet (t, s) in
        
        (* Briggs strategy *)
        let conservative ns = NS.fold ~init:0 ~f:(fun cnt n -> 
            if not (iscolorable (gtemp n) (adjacent n |> NS.to_list |> List.map ~f:gtemp)) then
                cnt + 1
            else
                cnt
        ) ns < 132
        in

        match MS.choose !worklistMoves with
        | Some ((x, y) as m) ->
            let x = getAlias x in
            let y = getAlias y in

            (* Swap "x" & "y" so that if "v" is precolored both "u" & "v" are and if "u" is at least "u" is *)
            let u, v = if NS.mem !precolored y then y, x else x, y in

            worklistMoves := MS.remove !worklistMoves m;

            (* 
                Trivial case where nodes "x" and "y" have already been coalesced to a same node "u" via other moves.
                There is no need to "combine" here.
            *)
            if u = v then (
                coalescedMoves := MS.add !coalescedMoves m;
                (* Simplification opportunity have been created as "u" might no longer been move related *)
                addWorkList u
            ) 
            
            (* The constrained case: both x & y move are precolored or they interfere *)
            else if NS.mem !precolored v || MS.mem !adjSet (u, v) then (
                (* Seb "constrainedMoves" is actually not required, we could simply discard "m" *)
                constrainedMoves := MS.add !constrainedMoves m;
                (* More simplification opportunities *)
                addWorkList u;
                addWorkList v;
            ) 
            
            (* George is expensive: we do it only for precolored nodes. Otherwise we do Brigggs *)
            else if (NS.mem !precolored u && NS.for_all ~f:(fun t -> ok(t, u)) (adjacent v))
                || (not (NS.mem !precolored u) && conservative (NS.union (adjacent u) (adjacent v))) then (
                coalescedMoves := MS.add !coalescedMoves m;
                combine (u, v);
                addWorkList u
            ) 
            
             (* 
                    That move nodes cannot be coalesced yet, put it appart in the activeMoves list.
                    We might put it back into the worklistMoves later on via enableMoves after some
                    nodes have been simplified.
            *)
            else (
                activeMoves := MS.add !activeMoves m
            )

        | None -> ()

    in
    
    let freezeMoves u = 
         u |> nodeMoves |> MS.iter ~f:(fun ((x, y) as m) -> 
            let v = if (getAlias u) = (getAlias y) then x else y in
            activeMoves := MS.remove !activeMoves m;
            frozenMoves := MS.add !frozenMoves m;
            if not (moveRelated v) && 
                iscolorable (gtemp v) (adjacent v |> NS.to_list |> List.map ~f:gtemp) then
            begin
                freezeWorklist := remove !freezeWorklist v;
                simplifyWorklist := v::!simplifyWorklist
            end
        )
    in
    
    let freeze () =
        (* 
            No more coalescing is possible at that stage.
            We got no choice but to freeze the moves of a node in the "freezeWorklist".
        *)
        match !freezeWorklist with
        | n::ns ->
            freezeWorklist := ns;
            simplifyWorklist := n::!freezeWorklist;
            freezeMoves n
        | [] -> ()
    in
    
    let selectSpill () =
        let n = List.fold ~init:None ~f:(fun acc n ->
            let c = color.spillCost n in
            match acc with 
            | Some (_, c') when c >= c' -> acc
            | _ -> Some (n, c)
        ) !spillWorklist
        in
        match n with
        | Some (n, _) ->
            spillWorklist := remove !spillWorklist n;
            simplifyWorklist := n::!simplifyWorklist;
            (* No coalescing for "n" as it might go into memory if it's an actual spill *)
            freezeMoves n
        | None -> ()
    in

    let spilledNodes = ref [] in

    let assignColors () = 
        
        !selectStack |> List.iter ~f:(fun n ->

            let okColors = ref (color.targetmodel.classes (gtemp n).regclass) in
            NS.iter ~f:(fun w -> 
                let a = getAlias w in
                match TT.look (!coloredNodes, (gtemp a).temp) with
                | Some c -> okColors := String.Set.partition_tf !okColors ~f:(color.targetmodel.conflict c) |> snd
                | None -> ()
            ) (NT.look_exn (!adjList, n));

            let t = (gtemp n).temp in
            match String.Set.choose !okColors with
            | Some c -> coloredNodes := TT.enter (!coloredNodes, t, c)
            | None -> spilledNodes := t::!spilledNodes
                
        );

        !coalescedNodes |> NS.iter ~f:(fun n ->
            let a = getAlias n in
            match TT.look (!coloredNodes, (gtemp a).temp) with
            | Some c -> coloredNodes := TT.enter (!coloredNodes, (gtemp n).temp, c)
            | None -> ()
        )
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