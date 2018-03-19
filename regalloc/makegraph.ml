module F = functor(Assem : Assem.T) -> struct

module Flow = Flowgraph.F(Assem.Temp)

open Flow
open Core
open Assem
open Graph

let dotGraph flowgraph = 
    Out_channel.print_endline "digraph G {";
    List.iter ~f:(fun n -> 
        List.iter ~f:(fun m ->
            Out_channel.print_endline ((Graph.nodename n) ^ " -> " ^ (Graph.nodename m) ^ ";");
        ) (Graph.succ n)
    ) flowgraph.control;
    Out_channel.print_endline "}"

let instrs2graph instrs =
    
    let create_nodes instrs = 

        let graph = Graph.newGraph () in

        let aux (fgraph, labToNode, lnodes, labels, ginstr) instr = 

            match instr with
            | OPER op ->
                
                let node = Graph.newNode graph in

                let fgraph = { 
                    control = [];
                    def = Graph.Table.enter (fgraph.def, node, op.dst);
                    use = Graph.Table.enter (fgraph.use, node, op.src);
                    ismove = Graph.Table.enter (fgraph.ismove, node, false)
                }
                in
                
                let labToNode = List.fold ~init:labToNode ~f:(fun t l -> Symbol.enter (t, l, node)) labels in
                
                (fgraph, labToNode, (node, op.jump)::lnodes, [], Graph.Table.enter (ginstr, node, instr))

            | MOVE mv ->
                
                let node = Graph.newNode graph in

                let fgraph = { 
                    control = [];
                    def = Graph.Table.enter (fgraph.def, node, [ mv.dst ]);
                    use = Graph.Table.enter (fgraph.use, node, [ mv.src ]);
                    ismove = Graph.Table.enter (fgraph.ismove, node, true)
                } 
                in

                let labToNode = List.fold ~init:labToNode ~f:(fun t l -> Symbol.enter (t, l, node)) labels in

                (fgraph, labToNode, (node, None)::lnodes, [], Graph.Table.enter (ginstr, node, instr))

            | LABEL lab -> (fgraph, labToNode, lnodes, lab.lab::labels, ginstr)
        in

        let (fgraph, labToNode, lnodes, labels, ginstr) = List.fold ~init:({ 
            control = []; 
            def = Graph.Table.empty; 
            use = Graph.Table.empty;
            ismove = Graph.Table.empty
        }, Symbol.empty, [], [], Graph.Table.empty) ~f:aux instrs
        in

        let (labToNode, lnodes) = 
            match labels with
            | [] -> (labToNode, lnodes)
            | _ -> 
                let node = Graph.newNode graph in
                let labToNode = List.fold ~init:labToNode ~f:(fun t l -> Symbol.enter (t, l, node)) labels in
                (labToNode, (node, None)::lnodes)
        in

        (fgraph, labToNode, List.rev lnodes, ginstr)
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

    let (flowgraph, labToNode, lnodes, ginstr) = create_nodes instrs in

    create_edges labToNode lnodes;

    let flowgraph =  {flowgraph with control = List.map ~f:(fun (n, _) -> n) lnodes} in

    dotGraph flowgraph;

    (flowgraph, (fun node -> Graph.Table.look_exn (ginstr, node)))

end