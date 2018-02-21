module Color : functor (Frame: Frame.T) ->
sig
(*module Temp = Frame.Temp*)
module Temp : Temp.T
module Flowgraph : Flowgraph.T with module Temp = Temp
module Liveness : Liveness.T with module Flow = Flowgraph
type allocation = Frame.register Temp.Table.table
type color = {interference: Liveness.igraph; initial: allocation; spillCost: Graph.node -> int; registers: Frame.register list} 
val color :  color -> allocation * Temp.temp list
end