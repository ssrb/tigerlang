module type T = sig

module Flow : Flowgraph.T
module Temp : Temp.T

type igraph = {
    graph: Graph.node list;
    tnode: Temp.temp -> Graph.node;
    gtemp: Graph.node -> Temp.temp;
    moves: (Graph.node * Graph.node) list
}

val interferenceGraph : Flow.flowgraph -> igraph

val show : Core.Out_channel.t -> igraph -> unit
end 

module F = functor(Flow : Flowgraph.T) -> struct

module Flow = Flow
module Temp = Flow.Temp

module TT = Temp.Table
module GT = Graph.Table

open Core
open Flow

type igraph = {
    graph: Graph.node list;
    tnode: Temp.temp -> Graph.node;
    gtemp: Graph.node -> Temp.temp;
    moves: (Graph.node * Graph.node) list
}

type liveSet = unit TT.table * Temp.temp list 
type liveMap = liveSet GT.table

let liveness flowgraph : liveMap =

    let aux (lins, louts, converged) node =
        let use = GT.look_exn (flowgraph.use, node) |> Temp.Set.of_list in
        let def = GT.look_exn (flowgraph.def, node) |> Temp.Set.of_list in
        let lin = GT.look_exn (lins, node) in
        let lout = GT.look_exn (louts, node) in
        let lin' = Temp.Set.union use (Temp.Set.diff lout def) in
        let lout' = Graph.succ node |> List.fold ~init:Temp.Set.empty ~f:(fun out succ -> 
            Temp.Set.union out (GT.look_exn (lins, succ))) 
        in
        let converged = converged && Temp.Set.equal lin lin' && Temp.Set.equal lout lout' in
        (GT.enter (lins, node, lin'), GT.enter (louts, node, lout'), converged)
    in 

    let rec iter nodes lins louts =
        let (lins, louts, converged) = List.fold ~init:(lins, louts, true) ~f:aux nodes in
        if converged then
            (lins, louts)
        else
            iter nodes lins louts
    in

    let nodes = flowgraph.control |> List.rev in

    let emptylivesets = nodes
        |> List.fold ~init:GT.empty ~f:(fun lin n ->
            GT.enter (lin, n, Temp.Set.empty) 
        )
    in

    let (_, louts) = iter nodes emptylivesets emptylivesets in

    nodes |> List.fold ~init:GT.empty ~f:(fun louts' n ->
        let l = GT.look_exn (louts, n) |> Set.to_list in
        let s = l |> List.fold ~init:TT.empty ~f:(fun s lo -> TT.enter (s, lo, ())) in
        GT.enter (louts', n, (s, l)))

let interferenceGraph flowgraph = 

    let louts = liveness flowgraph in

    let donode (igraph, tnode, gtemp, moves) n = 
        let (s, l) = GT.look_exn (louts, n) in
        let def = GT.look_exn (flowgraph.def, n) in
        let use = GT.look_exn (flowgraph.use, n) in
        let ismove = GT.look_exn (flowgraph.ismove, n) in
        assert((not ismove) || (List.length def = 1 && List.length use = 1));
        let (tnode, gtemp, moves) = def |> List.fold ~init:(tnode, gtemp, moves) ~f:(fun _ d ->
            let node = Graph.newNode igraph in
            let tnode = TT.enter (tnode, d, node) in
            let gtemp = GT.enter (gtemp, node, d) in
            let moves = l |> List.fold ~init:moves ~f:(fun moves out -> 
                match TT.look (tnode, out) with
                | Some node' ->
                    if not ismove then begin
                        Graph.mk_edge Graph.{f = node; t= node'};
                        moves
                    end else begin
                        if List.hd_exn use <> out then
                            Graph.mk_edge Graph.{f = node; t= node'};
                        (node, node')::moves
                    end
                | None -> moves
            )
            in
            (tnode, gtemp, moves)
        )
        in 
        (igraph, tnode, gtemp, moves)
    in

    let init = (Graph.newGraph (), TT.empty, GT.empty, []) in

    let (graph, tnode, gtemp, moves) = flowgraph.control
        |> List.fold ~init ~f:donode
    in
    
    {
        graph = Graph.nodes graph;
        tnode = (fun t -> TT.look_exn (tnode, t));
        gtemp = (fun n -> GT.look_exn (gtemp, n));
        moves = moves
    }

let show outstream graph =

    Out_channel.output_string outstream "graph G {";
    Out_channel.newline outstream;
    graph.graph |> List.iter ~f:(fun n ->
        let ntmp = n |> graph.gtemp |> Temp.makestring in
        Graph.succ n |> List.iter ~f:(fun m ->
            let mtmp = m |> graph.gtemp |> Temp.makestring in
            Out_channel.output_string outstream (ntmp ^ "--" ^ mtmp);
            Out_channel.newline outstream
        )
    );
    Out_channel.output_string outstream "}";
    Out_channel.newline outstream


end