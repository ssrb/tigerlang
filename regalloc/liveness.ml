module F = functor(Flow : Flowgraph.T) -> struct
module Temp = Flow.Temp

type igraph = {
    graph: Graph.graph;
    tnode: Temp.temp -> Graph.node;
    gtemp: Graph.node -> Temp.temp;
    moves: (Graph.node * Graph.node) list
}

let interferenceGraph flowgraph = ({
    graph = Graph.newGraph ();
    tnode = (fun _ -> Graph.newNode (Graph.newGraph ()));
    gtemp = (fun _ -> Temp.newtemp ());
    moves = []
}, (fun _ -> []))


let show (outstream, graph) = ()

end