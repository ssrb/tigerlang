module type T = sig
module Temp : Temp.T
module Frame : Frame.T
module Liveness : Liveness.T
type allocation = Frame.register Temp.Table.table
type color = {interference: Liveness.igraph; initial: allocation; spillCost: Graph.node -> float; registers: Frame.register list} 
val color :  color -> allocation * Temp.temp list
end

module F (Frame: Frame.T) (Liveness: Liveness.T with module Temp = Frame.Temp) : T with module Frame = Frame and module Liveness = Liveness and module Temp = Frame.Temp