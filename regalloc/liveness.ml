module type T = sig

module Flow : Flowgraph.T
module Temp : Temp.T

type igraph = {
    graph: Graph.graph;
    tnode: Temp.temp -> Graph.node;
    gtemp: Graph.node -> Temp.temp;
    moves: (Graph.node * Graph.node) list
}

val interferenceGraph : Flow.flowgraph -> igraph * (Graph.node -> Temp.temp list)

val show : Core.Out_channel.t * igraph -> unit
end 

module F = functor(Flow : Flowgraph.T) -> struct

module Flow = Flow
module Temp = Flow.Temp

open Core
open Graph
open Flow

type igraph = {
    graph: Graph.graph;
    tnode: Temp.temp -> Graph.node;
    gtemp: Graph.node -> Temp.temp;
    moves: (Graph.node * Graph.node) list
}

type liveSet = unit Temp.Table.table * Temp.temp list 
type liveMap = liveSet Graph.Table.table

let liveness flowgraph : liveMap =

    let module Comp = Comparator.Make (
        struct
            type t = Temp.temp [@@deriving sexp]
            let compare = Temp.cmptemp
        end
    ) 
    in

    let cmp = Comp.comparator in

    let aux (lins, louts, converged) node =
        let use = Option.value_exn (Graph.Table.look (flowgraph.use, node)) |> Set.of_list ~comparator:cmp in
        let def = Option.value_exn (Graph.Table.look (flowgraph.def, node)) |> Set.of_list ~comparator:cmp in
        let lin = Option.value_exn (Graph.Table.look (lins, node)) in
        let lout = Option.value_exn (Graph.Table.look (louts, node)) in
        let lin' = Set.union use (Set.diff lout def) in
        let lout' = Graph.succ node |> List.fold ~init:(Set.empty ~comparator:cmp) ~f:(fun out succ -> 
            Set.union out (Option.value_exn (Graph.Table.look (lins, succ)))) 
        in
        let converged = converged && Set.equal lin lin' && Set.equal lout lout' in
        (Graph.Table.enter (lins, node, lin'), Graph.Table.enter (louts, node, lout'), converged)
    in 

    let rec iter nodes lins louts =
        let (lins, louts, converged) = List.fold ~init:(lins, louts, true) ~f:aux nodes in
        if converged then
            (lins, louts)
        else
            iter nodes lins louts
    in

    let nodes = Graph.nodes flowgraph.control |> List.rev in

    let emptylivesets = nodes
        |> List.fold ~init:Graph.Table.empty ~f:(fun lin n ->
            Graph.Table.enter (lin, n, Set.empty ~comparator:cmp) 
        )
    in

    let (_, louts) = iter nodes emptylivesets emptylivesets in

    nodes |> List.fold ~init:Graph.Table.empty ~f:(fun louts' n ->
        let l = Option.value_exn (Graph.Table.look (louts, n)) |> Set.to_list in
        let s = l |> List.fold ~init:Temp.Table.empty ~f:(fun s lo -> Temp.Table.enter (s, lo, ())) in
        Graph.Table.enter (louts', n, (s, l)))

let interferenceGraph flowgraph = 

    let louts = liveness flowgraph in

    let donode (igraph, tnode, gtemp, moves) n = 
        let (s, l) = Option.value_exn (Graph.Table.look (louts, n)) in
        let def = Option.value_exn (Graph.Table.look (flowgraph.def, n)) in
        let ismove = Option.value_exn (Graph.Table.look (flowgraph.ismove, n)) in
        let (tnode, gtemp, moves) = def |> List.fold ~init:(tnode, gtemp, moves) ~f:(fun _ d ->
            let node = Graph.newNode igraph in
            let tnode = Temp.Table.enter (tnode, d, node) in
            let gtemp = Graph.Table.enter (gtemp, node, d) in
            let moves = l |> List.fold ~init:moves ~f:(fun moves out -> 
                let node' = Option.value_exn (Temp.Table.look (tnode, out)) in
                if not ismove then begin
                    Graph.mk_edge {f = node; t= node'};
                    moves
                end else begin
                    if d <> out then
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

    let init = (Graph.newGraph (), Temp.Table.empty, Graph.Table.empty, []) in

    let (graph, tnode, gtemp, moves) = Graph.nodes flowgraph.control
        |> List.fold ~init ~f:donode
    in
    
    ({
        graph = graph;
        tnode = (fun t -> Option.value_exn (Temp.Table.look (tnode, t)));
        gtemp = (fun n -> Option.value_exn (Graph.Table.look (gtemp, n)));
        moves = moves
    }, (fun n -> snd (Option.value_exn (Graph.Table.look (louts, n)))))

let show (outstream, graph) = ()

end