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

val show : Core.Out_channel.t * igraph -> unit
end 

module F = functor(Flow : Flowgraph.T) -> struct

module Flow = Flow
module Temp = Flow.Temp

module TT = Temp.Table
module GT = Graph.Table

open Core
open Graph
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

    let cmp = Temp.Comp.comparator in

    let aux (lins, louts, converged) node =
        let use = Option.value_exn (GT.look (flowgraph.use, node)) |> Set.of_list ~comparator:cmp in
        let def = Option.value_exn (GT.look (flowgraph.def, node)) |> Set.of_list ~comparator:cmp in
        let lin = Option.value_exn (GT.look (lins, node)) in
        let lout = Option.value_exn (GT.look (louts, node)) in
        let lin' = Set.union use (Set.diff lout def) in
        let lout' = Graph.succ node |> List.fold ~init:(Set.empty ~comparator:cmp) ~f:(fun out succ -> 
            Set.union out (Option.value_exn (GT.look (lins, succ)))) 
        in
        let converged = converged && Set.equal lin lin' && Set.equal lout lout' in
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
            GT.enter (lin, n, Set.empty ~comparator:cmp) 
        )
    in

    let (_, louts) = iter nodes emptylivesets emptylivesets in

    nodes |> List.fold ~init:GT.empty ~f:(fun louts' n ->
        let l = Option.value_exn (GT.look (louts, n)) |> Set.to_list in
        let s = l |> List.fold ~init:TT.empty ~f:(fun s lo -> TT.enter (s, lo, ())) in
        GT.enter (louts', n, (s, l)))

let interferenceGraph flowgraph = 

    let louts = liveness flowgraph in

    let donode (igraph, tnode, gtemp, moves) n = 
        let (s, l) = Option.value_exn (GT.look (louts, n)) in
        let def = Option.value_exn (GT.look (flowgraph.def, n)) in
        let use = Option.value_exn (GT.look (flowgraph.use, n)) in
        let ismove = Option.value_exn (GT.look (flowgraph.ismove, n)) in
        assert((not ismove) || (List.length def = 1 && List.length use = 1));
        let (tnode, gtemp, moves) = def |> List.fold ~init:(tnode, gtemp, moves) ~f:(fun _ d ->
            let node = Graph.newNode igraph in
            let tnode = TT.enter (tnode, d, node) in
            let gtemp = GT.enter (gtemp, node, d) in
            let moves = l |> List.fold ~init:moves ~f:(fun moves out -> 
                let node' = Option.value_exn (TT.look (tnode, out)) in
                if not ismove then begin
                    Graph.mk_edge {f = node; t= node'};
                    moves
                end else begin
                    if List.hd_exn use <> out then
                        Graph.mk_edge {f = node; t= node'};
                    (node, node')::moves
                end
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
        tnode = (fun t -> Option.value_exn (TT.look (tnode, t)));
        gtemp = (fun n -> Option.value_exn (GT.look (gtemp, n)));
        moves = moves
    }

let show (outstream, graph) =
    graph.graph |> List.iter ~f:(fun n ->
        n |> Graph.nodename |> Out_channel.output_string outstream;
        Out_channel.output_string outstream ":";
        Graph.adj n |> List.iter ~f:(fun n ->
            Out_channel.output_string outstream " ";
            n |> Graph.nodename |> Out_channel.output_string outstream;
        );
        Out_channel.newline outstream
    )

end