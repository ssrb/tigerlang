module F = functor(Frame : Frame.T) ->
struct
module Temp = Frame.Temp
module Assem = Frame.Assem

module Makegraph = Makegraph.F(Assem)
module Liveness = Liveness.F(Makegraph.Flow)
module Color = Color.F (Frame) (Liveness)

open Color

let alloc (asm, (frame : Frame.frame)) = 
    let fgraph =  Makegraph.instrs2graph asm in
    let igraph, liveouts = Liveness.interferenceGraph fgraph in
	let alloc, spills = Color.color {interference = igraph; initial = Temp.Table.empty; spillCost = (fun _ -> 0); registers = Frame.registers} in
    (asm, alloc)

end