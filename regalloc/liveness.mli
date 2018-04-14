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

module F : functor(Flow : Flowgraph.T) -> T with module Flow = Flow and module Assem = Flow.Assem
