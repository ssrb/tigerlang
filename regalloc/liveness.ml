module type T = sig

module Flow : Flowgraph.T
module Assem : Assem.T

type igraph = {
    graph: Graph.node list;
    tnode: Assem.Variable.t -> Graph.node;
    gtemp: Graph.node -> Assem.Variable.t;
    moves: (Graph.node * Graph.node) list
}

val interferenceGraph : Flow.flowgraph -> igraph

val show : Core.Out_channel.t -> igraph -> ?color:(Graph.node -> string) -> unit
end 

module F = functor(Flow : Flowgraph.T) -> struct

module Flow = Flow
module Assem = Flow.Assem
module Temp = Assem.Temp

module TT = Temp.Table
module VS = Assem.Variable.Set
module GT = Graph.Table

open Core
open Flow

type igraph = {
    graph: Graph.node list;
    tnode: Assem.Variable.t -> Graph.node;
    gtemp: Graph.node -> Assem.Variable.t;
    moves: (Graph.node * Graph.node) list
}

let liveness flowgraph =
    
    let nodes = flowgraph.control |> List.rev in

    let defs = nodes |> List.fold ~init:GT.empty ~f:(fun t n ->
        GT.enter (t, n, GT.look_exn (flowgraph.def, n) |> VS.of_list)
    )
    in

    let uses = nodes |> List.fold ~init:GT.empty ~f:(fun t n ->
        GT.enter (t, n, GT.look_exn (flowgraph.use, n) |> VS.of_list)
    )
    in

    let aux (lins, louts, converged) node =
        let def = GT.look_exn (defs, node) in
        let use = GT.look_exn (uses, node) in
        let lin = GT.look_exn (lins, node) in
        let lout = GT.look_exn (louts, node) in
        let lin' = VS.union use (VS.diff lout def) in
        let lout' = node 
            |> Graph.succ 
            |> List.fold ~init:VS.empty ~f:(fun out succ -> 
                VS.union out (GT.look_exn (lins, succ))
            ) 
        in
        let converged = converged && VS.equal lin lin' && VS.equal lout lout' in
        (GT.enter (lins, node, lin'), GT.enter (louts, node, lout'), converged)
    in 

    let rec iter nodes lins louts =
        let (lins, louts, converged) = List.fold ~init:(lins, louts, true) ~f:aux nodes in
        if converged then
            (lins, louts)
        else
            iter nodes lins louts
    in

    let emptylivesets = nodes
        |> List.fold ~init:GT.empty ~f:(fun lin n ->
            GT.enter (lin, n, VS.empty) 
        )
    in

    let (_lins, louts) = iter nodes emptylivesets emptylivesets in

    louts

let interferenceGraph flowgraph = 

    let louts = liveness flowgraph in

    let donode (igraph, tnode, gtemp, moves) n =

        let tempnode ((var : Assem.Variable.t), tnode, gtemp) =
            match TT.look (tnode, var.temp) with
            | Some node -> node, tnode, gtemp
            | None ->
                let node = Graph.newNode igraph in
                node, TT.enter (tnode, var.temp, node), GT.enter (gtemp, node, var)
        in

        let louts = GT.look_exn (louts, n) in
        let def = GT.look_exn (flowgraph.def, n) in
        let use = GT.look_exn (flowgraph.use, n) in
        let ismove = GT.look_exn (flowgraph.ismove, n) in

        assert((not ismove) || (List.length def = 1 && List.length use = 1));

        let (tnode, gtemp, moves) = def |> List.fold ~init:(tnode, gtemp, moves) ~f:(fun (tnode, gtemp, moves) d ->

            let (node, tnode, gtemp) = tempnode (d, tnode, gtemp) in

            let (tnode, gtemp) = louts |> VS.fold ~init:(tnode, gtemp) ~f:(fun (tnode, gtemp) out -> 

                let (node', tnode, gtemp) = tempnode (out, tnode, gtemp) in

                if node <> node' then (
                    if not ismove then
                        Graph.(mk_edge {f = node; t = node'})
                    else if (List.hd_exn use) <> out then
                        Graph.(mk_edge {f = node; t= node'})
                );

                (tnode, gtemp)
            )
            in

            if ismove then
                let (node', tnode, gtemp) = tempnode (List.hd_exn use, tnode, gtemp) in
                (tnode, gtemp, (node, node')::moves)
            else
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
        tnode = (fun t -> TT.look_exn (tnode, t.temp));
        gtemp = (fun n -> GT.look_exn (gtemp, n));
        moves = moves
    }

let show outstream graph ?(color=(fun _ -> "black")) =

    let makestring (v : Assem.Variable.t) = Temp.makestring v.temp in 

    Out_channel.output_string outstream "graph G {";
    Out_channel.newline outstream;
    graph.graph |> List.iter ~f:(fun n ->
        let ntmp = n |> graph.gtemp |> makestring in
        Out_channel.output_string outstream (ntmp ^ "[color=" ^ (color n) ^ "]");
        Out_channel.newline outstream;
        Graph.succ n |> List.iter ~f:(fun m ->
                let mtmp = m |> graph.gtemp |> makestring in
                Out_channel.output_string outstream (ntmp ^ "--" ^ mtmp);
                Out_channel.newline outstream
        )
    );
    graph.moves |> List.iter ~f:(fun (n, m) ->
        let n = n |> graph.gtemp |> makestring in
        let m = m |> graph.gtemp |> makestring in
        Out_channel.output_string outstream (n ^ "--" ^ m ^ "[style=dashed]");
        Out_channel.newline outstream
        
    );
    Out_channel.output_string outstream "}";
    Out_channel.newline outstream


end