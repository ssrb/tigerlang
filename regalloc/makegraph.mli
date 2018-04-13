module F : functor(Assem : Assem.T) -> sig
module Flow : Flowgraph.T with module Assem = Assem
val instrs2graph : Assem.instr list -> Flow.flowgraph * (Graph.node -> Assem.instr)
end