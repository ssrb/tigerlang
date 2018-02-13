module F = functor(Assem : Assem.T) -> struct

module Flow = Flowgraph.F(Assem.Temp)

open Flow
open Core
open Assem
open Graph

let instrs2graph instrs =
    let f (fgraph, nodes) instr = 
        match instr with 
        | OPER op -> 
            let node = Graph.newNode fgraph.control in
            Graph.mk_edge {f = node; t = node};
            let fgraph = { 
                fgraph with 
                def = Graph.Table.enter (fgraph.def, node, op.dst);
                use = Graph.Table.enter (fgraph.use, node, op.dst);
                ismove = Graph.Table.enter (fgraph.ismove, node, false)
            } in
            (fgraph, node::nodes)
	    | LABEL lab -> (fgraph, nodes)
	    | MOVE mv ->
            let node = Graph.newNode fgraph.control in
            Graph.mk_edge {f = node; t = node};
            let fgraph = { 
                fgraph with 
                def = Graph.Table.enter (fgraph.def, node, [ mv.dst ]);
                use = Graph.Table.enter (fgraph.use, node, [ mv.dst ]);
                ismove = Graph.Table.enter (fgraph.ismove, node, true)
            } in
            (fgraph, node::nodes)
    in List.fold ~init:({ 
        control = Graph.newGraph (); 
        def = Graph.Table.empty; 
        use = Graph.Table.empty;
        ismove = Graph.Table.empty
    }, []) ~f:f instrs

end