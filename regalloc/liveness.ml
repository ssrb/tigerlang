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

let interferenceGraph flowgraph = 

    let module Comp = Comparator.Make(
        struct
            type t = Temp.temp [@@deriving sexp]
            let compare = Temp.cmptemp
        end
    ) in

    let cmp = Comp.comparator in

    let nodes = Graph.nodes flowgraph.control |> List.rev in

    let emptyliveset = 
        nodes
        |> List.fold ~init:Graph.Table.empty ~f:(fun lin n ->
            Graph.Table.enter (lin, n, Set.empty ~comparator:cmp) 
        )
    in

    let lin = emptyliveset in

    let lout = emptyliveset in
    
    let liveness nodes lin lout =
        let aux (lin, lout) node =
            let use = Option.value_exn (Graph.Table.look (flowgraph.use, node)) |> Set.of_list ~comparator:cmp in
            let def = Option.value_exn (Graph.Table.look (flowgraph.def, node)) |> Set.of_list ~comparator:cmp in
            (Set.union use (Set.diff lout def), Graph.succ node |> List.fold ~init:(Set.empty ~comparator:cmp) ~f:(fun out succ -> 
                Set.union out (Option.value_exn (Graph.Table.look (lin, node)))))
        in ()
    in
({
    graph = Graph.newGraph ();
    tnode = (fun _ -> Graph.newNode (Graph.newGraph ()));
    gtemp = (fun _ -> Temp.newtemp ());
    moves = []
}, (fun _ -> []))

let show (outstream, graph) = ()

end