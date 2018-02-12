module F = functor(Assem : Assem.T) -> struct
module Flow = Flowgraph.F(Assem.Temp)
open Flow
let instrs2graph instrs = (
    { 
        control = Graph.newGraph (); 
        def = Graph.Table.empty; 
        use = Graph.Table.empty;
        ismove = Graph.Table.empty
    }
    , [])
end