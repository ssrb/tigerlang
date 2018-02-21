module Color = functor (Frame: Frame.T) ->
struct
module Temp = Frame.Temp
module Flowgraph = Flowgraph.F(Temp)
module Liveness = Liveness.F(Flowgraph)
type allocation = Frame.register Temp.Table.table
type color = {interference: Liveness.igraph; initial: allocation; spillCost: Graph.node -> int; registers: Frame.register list} 
let color _ = (Temp.Table.empty, [])
end