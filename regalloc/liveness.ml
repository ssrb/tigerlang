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
open Flow

type igraph = {
    graph: Graph.graph;
    tnode: Temp.temp -> Graph.node;
    gtemp: Graph.node -> Temp.temp;
    moves: (Graph.node * Graph.node) list
}

type liveSet = unit Temp.Table.table * Temp.temp list 
type liveMap = liveSet Graph.Table.table

let interferenceGraph flowgraph = 

    let liveness flowgraph =

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

        let (lins, louts) = iter nodes emptylivesets emptylivesets in

        let lins = nodes |> List.fold ~init:Graph.Table.empty ~f:(fun lins' n ->
            Graph.Table.enter (lins', n, Option.value_exn (Graph.Table.look (lins, n)) |> Set.to_list))
        in

        let louts = nodes |> List.fold ~init:Graph.Table.empty ~f:(fun louts' n ->
            Graph.Table.enter (louts', n, Option.value_exn (Graph.Table.look (louts, n)) |> Set.to_list))
        in

        (lins, louts)

    in
({
    graph = Graph.newGraph ();
    tnode = (fun _ -> Graph.newNode (Graph.newGraph ()));
    gtemp = (fun _ -> Temp.newtemp ());
    moves = []
}, (fun _ -> []))

let show (outstream, graph) = ()

end