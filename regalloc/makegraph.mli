module F : functor(Assem : Assem.T) -> sig
module Flow : Flowgraph.T with module Temp = Assem.Temp
val instrs2graph : Assem.instr list -> Flow.flowgraph * Graph.node list
end