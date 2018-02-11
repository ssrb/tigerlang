module F : functor(Flow : Flowgraph.T) -> sig 
module Temp = Flow.Temp
module IGraph = Graph
type igraph = {
    grap: IGraph.graph;
    tnode: Temp.temp -> IGraph.node;
    gtemp: IGraph.node -> Temp.temp;
    moves: (IGraph.node * IGraph.node) list
}

val interferenceGraph : Flow.flowgraph -> igraph * (Flow.graph.node -> Temp.temp list)

val show : Core.Out_channel.t * igraph -> unit
end