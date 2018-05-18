module type T = sig
module Temp : Temp.T
module Frame : Frame.T
module Liveness : Liveness.T
type allocation = Frame.register Temp.Table.table
type color = {interference: Liveness.igraph; initial: allocation; spillCost: Graph.node -> float; targetmodel: Frame.targetmodel}
val color :  color -> allocation * Liveness.Assem.Variable.t list
end

module F (Frame: Frame.T) (Liveness: Liveness.T with module Assem = Frame.Assem) : T with module Frame = Frame and module Liveness = Liveness and module Temp = Frame.Temp