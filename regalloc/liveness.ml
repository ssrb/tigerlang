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
module TS = Temp.Set
module GT = Graph.Table

open Core
open Flow

type igraph = {
    graph: Graph.node list;
    tnode: Temp.temp -> Graph.node;
    gtemp: Graph.node -> Temp.temp;
    moves: (Graph.node * Graph.node) list
}

type liveSet = TS.t * Temp.temp list 
type liveMap = liveSet GT.table

let liveness flowgraph : liveMap =
    
    let aux (lins, louts, converged) node =
        let def = GT.look_exn (flowgraph.def, node) |> TS.of_list in
        let use = GT.look_exn (flowgraph.use, node) |> TS.of_list in
        let lin = GT.look_exn (lins, node) in
        let lout = GT.look_exn (louts, node) in
        let lin' = TS.union use (TS.diff lout def) in
        let lout' = node 
            |> Graph.succ 
            |> List.fold ~init:TS.empty ~f:(fun out succ -> 
                TS.union out (GT.look_exn (lins, succ))
            ) 
        in
        let converged = converged && TS.equal lin lin' && TS.equal lout lout' in
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

    let (_lins, louts) = iter nodes emptylivesets emptylivesets in

    List.fold ~init:GT.empty ~f:(fun louts' n ->
        let s = GT.look_exn (louts, n) in
        let l = TS.to_list s in
        GT.enter (louts', n, (s, l))
    ) nodes

let interferenceGraph flowgraph = 

    let louts = liveness flowgraph in

    let donode (igraph, tnode, gtemp, moves) n = 
        let (louts, _) = GT.look_exn (louts, n) in
        let def = GT.look_exn (flowgraph.def, n) in
        let use = GT.look_exn (flowgraph.use, n) in
        let ismove = GT.look_exn (flowgraph.ismove, n) in
        assert((not ismove) || (List.length def = 1 && List.length use = 1));
        let (tnode, gtemp, moves) = def |> List.fold ~init:(tnode, gtemp, moves) ~f:(fun (tnode, gtemp, moves) d ->

            let node, tnode, gtemp = 
            match TT.look (tnode, d) with
            | Some node -> node, tnode, gtemp
            | None ->
                let node = Graph.newNode igraph in
                node, TT.enter (tnode, d, node), GT.enter (gtemp, node, d)
            in

            let moves = louts |> TS.fold ~init:moves ~f:(fun moves out -> 
                match TT.look (tnode, out) with
                | Some node' when node <> node' ->
                    if not ismove then (
                        Graph.(mk_edge {f = node; t = node'});
                        moves
                    ) else (
                        if List.hd_exn use <> out then
                            Graph.(mk_edge {f = node; t= node'});
                        (node, node')::moves
                    )
                | _ -> moves
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