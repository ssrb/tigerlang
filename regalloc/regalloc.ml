module F = functor(Frame : Frame.T) ->
struct

module Temp = Frame.Temp
module Assem = Frame.Assem

module Makegraph = Makegraph.F(Assem)
module Liveness = Liveness.F(Makegraph.Flow)
module Color = Color.F (M68kFrame) (Liveness)

type allocation = Frame.register Temp.Table.table
let alloc _ = ([], Temp.Table.empty)

(*
let (igraph, _) = Liveness.interferenceGraph graph in

		ignore(Color.color {interference = igraph; initial = M68kTemp.Table.empty; spillCost = (fun _ -> 0); registers = M68kFrame.registers});
*)

end