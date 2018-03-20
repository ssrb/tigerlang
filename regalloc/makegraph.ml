module F = functor(Assem : Assem.T) -> struct

module Flow = Flowgraph.F(Assem.Temp)
module GT = Graph.Table

open Flow
open Core
open Assem

let instrs2graph instrs =
    
    let create_nodes instrs = 

        let graph = Graph.newGraph () in

        let aux (fgraph, labToNode, lnodes, labels, ginstr) instr = 

            match instr with
            | OPER op ->
                
                let node = Graph.newNode graph in

                let fgraph = { 
                    control = [];
                    def = GT.enter (fgraph.def, node, op.dst);
                    use = GT.enter (fgraph.use, node, op.src);
                    ismove = GT.enter (fgraph.ismove, node, false)
                }
                in
                
                let labToNode = List.fold ~init:labToNode ~f:(fun t l -> Symbol.enter (t, l, node)) labels in
                
                (fgraph, labToNode, (node, op.jump)::lnodes, [], GT.enter (ginstr, node, instr))

            | MOVE mv ->
                
                let node = Graph.newNode graph in

                let fgraph = { 
                    control = [];
                    def = GT.enter (fgraph.def, node, [ mv.dst ]);
                    use = GT.enter (fgraph.use, node, [ mv.src ]);
                    ismove = GT.enter (fgraph.ismove, node, true)
                } 
                in

                let labToNode = List.fold ~init:labToNode ~f:(fun t l -> Symbol.enter (t, l, node)) labels in

                (fgraph, labToNode, (node, None)::lnodes, [], GT.enter (ginstr, node, instr))

            | LABEL lab -> (fgraph, labToNode, lnodes, lab.lab::labels, ginstr)
        in

        let (fgraph, labToNode, lnodes, labels, ginstr) = List.fold ~init:({ 
            control = []; 
            def = GT.empty; 
            use = GT.empty;
            ismove = GT.empty
        }, Symbol.empty, [], [], GT.empty) ~f:aux instrs
        in

        let (fgraph, labToNode, lnodes, ginstr) = 
            match labels with
            | [] -> (fgraph, labToNode, lnodes, ginstr)
            | _ -> 
                let node = Graph.newNode graph in
                let labToNode = List.fold ~init:labToNode ~f:(fun t l -> Symbol.enter (t, l, node)) labels in
                let fgraph = { 
                    control = [];
                    def = GT.enter (fgraph.def, node, []);
                    use = GT.enter (fgraph.use, node, []);
                    ismove = GT.enter (fgraph.ismove, node, false)
                } 
                in
                (fgraph, labToNode, (node, None)::lnodes, GT.enter (ginstr, node, Assem.OPER {assem = "nop"; dst = []; src = []; jump = None}))
        in

        (fgraph, labToNode, List.rev lnodes, ginstr)
    in

    let rec create_edges labToNode lnodes =
        match lnodes with 
        | [] -> ()

        | (n, Some ls)::lnodes ->
            List.iter ~f:(fun l ->
                match Symbol.look (labToNode, l) with
                | Some n' -> Graph.(mk_edge {f = n; t = n'})
                | None -> assert(false)
            ) ls;
            create_edges labToNode lnodes
        
        | (n, None)::(((n', _)::_) as lnodes) -> 
            Graph.(mk_edge {f = n; t = n'});
            create_edges labToNode lnodes
        
        | _ ->  ()
    in

    let (flowgraph, labToNode, lnodes, ginstr) = create_nodes instrs in

    create_edges labToNode lnodes;

    let flowgraph =  {flowgraph with control = List.map ~f:(fun (n, _) -> n) lnodes} in

    let ginstr = (fun node -> GT.look_exn (ginstr, node)) in

    (flowgraph, ginstr)

end