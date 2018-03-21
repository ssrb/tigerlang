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

val show : Core.Out_channel.t -> igraph -> (Graph.node -> string) -> unit
end 

module F : functor(Flow : Flowgraph.T) -> T with module Flow = Flow and module Temp = Flow.Temp 
