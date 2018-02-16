module F = functor(Assem : Assem.T) -> struct

module Flow = Flowgraph.F(Assem.Temp)

open Flow
open Core
open Assem
open Graph

let instrs2graph instrs =
    
    let create_nodes instrs = 

        let aux (fgraph, labToNode, lnodes, labels) instr = 

            match instr with
            | OPER op ->
                
                let node = Graph.newNode fgraph.control in

                let fgraph = { 
                    fgraph with
                    def = Graph.Table.enter (fgraph.def, node, op.dst);
                    use = Graph.Table.enter (fgraph.use, node, op.src);
                    ismove = Graph.Table.enter (fgraph.ismove, node, false)
                }
                in
                
                let labToNode = List.fold ~init:labToNode ~f:(fun t l -> Symbol.enter (t, l, node)) labels in
                
                (fgraph, labToNode, (node, op.jump)::lnodes, [])

            | MOVE mv ->
                
                let node = Graph.newNode fgraph.control in

                let fgraph = { 
                    fgraph with 
                    def = Graph.Table.enter (fgraph.def, node, [ mv.dst ]);
                    use = Graph.Table.enter (fgraph.use, node, [ mv.src ]);
                    ismove = Graph.Table.enter (fgraph.ismove, node, true)
                } 
                in

                let labToNode = List.fold ~init:labToNode ~f:(fun t l -> Symbol.enter (t, l, node)) labels in

                (fgraph, labToNode, (node, None)::lnodes, [])

            | LABEL lab -> (fgraph, labToNode, lnodes, lab.lab::labels)
        in

        let (fgraph, labToNode, lnodes, labels) = List.fold ~init:({ 
            control = Graph.newGraph (); 
            def = Graph.Table.empty; 
            use = Graph.Table.empty;
            ismove = Graph.Table.empty
        }, Symbol.empty, [], []) ~f:aux instrs
        in

        let (labToNode, lnodes) = 
            match labels with
            | [] -> (labToNode, lnodes)
            | _ -> 
                let node = Graph.newNode fgraph.control in
                let labToNode = List.fold ~init:labToNode ~f:(fun t l -> Symbol.enter (t, l, node)) labels in
                (labToNode, (node, None)::lnodes)
        in

        (fgraph, labToNode, List.rev lnodes)
    in

    let rec create_edges labToNode lnodes =
        match lnodes with 
        | [] -> ()

        | (n, Some ls)::lnodes ->
            List.iter ~f:(fun l ->
                match Symbol.look (labToNode, l) with
                | Some n' -> Graph.mk_edge {f = n; t = n'}
                | None -> assert(false)
            ) ls;
            create_edges labToNode lnodes
        
        | (n, None)::(((n', _)::_) as lnodes) -> 
            Graph.mk_edge {f = n; t = n'};
            create_edges labToNode lnodes
        
        | _ ->  ()
    in

    let (flowgraph, labToNode, lnodes) = create_nodes instrs in

    create_edges labToNode lnodes;
    
    (flowgraph, List.map ~f:(fun (n, _) -> n) lnodes)

end