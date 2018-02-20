signature COLOR =
sig
structure Frane : Frame
type allocatoin  = Frame.register Temp.Table.Table
val color : {interference: Liveness,igraph,
initial: allocation,
spillCost: Graph.node -> int,
registers: Frame.register list} 
-> allocation * Temp.temp list
end