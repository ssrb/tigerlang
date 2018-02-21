module F : functor(Frame : Frame.T) ->
sig
    module Temp : Temp.T
    module Assem : Assem.T with module Temp = Temp
    type allocation = Frame.register Temp.Table.table
    val alloc : Assem.instr list * Frame.frame -> Assem.instr list * allocation
end with module Temp = Frame.Temp