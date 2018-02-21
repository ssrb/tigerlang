module F (Frame: Frame.T) (Liveness: Liveness.T) =
struct
module Temp = Frame.Temp
type allocation = Frame.register Temp.Table.table
type color = {interference: Liveness.igraph; initial: allocation; spillCost: Graph.node -> int; registers: Frame.register list} 
let color color  = (Temp.Table.empty, [])
end