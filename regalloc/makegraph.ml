module F = functor(Assem : Assem.T) -> struct

module Flow = Flowgraph.F(Assem.Temp)

open Flow
open Core
open Assem

let instrs2graph instrs =
    let f (fgraph, nodes) instr = 
        match instr with 
        | OPER op -> 
            let node = Graph.newNode fgraph.control in
            let fgraph = { 
                fgraph with 
                def = Graph.Table.enter (fgraph.def, node, op.dst);
                use = Graph.Table.enter (fgraph.use, node, op.dst);
                ismove = Graph.Table.enter (fgraph.ismove, node, false)
            } in
            (fgraph, node::nodes)
	    | LABEL lab -> (fgraph, nodes)
	    | MOVE mv -> (fgraph, nodes)
    in List.fold ~init:({ 
        control = Graph.newGraph (); 
        def = Graph.Table.empty; 
        use = Graph.Table.empty;
        ismove = Graph.Table.empty
    }, []) ~f:f instrs

end