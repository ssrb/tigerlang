module F (Frame: Frame.T) (Liveness: Liveness.T) :
sig
module Temp : Temp.T
type allocation = Frame.register Temp.Table.table
type color = {interference: Liveness.igraph; initial: allocation; spillCost: Graph.node -> int; registers: Frame.register list} 
val color :  color -> allocation * Temp.temp list
end with module Temp = Frame.Temp