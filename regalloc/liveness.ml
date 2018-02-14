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
    let lin = 
        Graph.nodes flowgraph.control
        |> List.fold ~init:Graph.Table.empty ~f:(fun lin n ->
            Graph.Table.enter (lin, n, []) 
        )
    in
    let lout = lin in
({
    graph = Graph.newGraph ();
    tnode = (fun _ -> Graph.newNode (Graph.newGraph ()));
    gtemp = (fun _ -> Temp.newtemp ());
    moves = []
}, (fun _ -> []))

let show (outstream, graph) = ()

end