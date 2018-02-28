module F : functor(Frame : Frame.T) ->
sig
    module Temp : Temp.T
    module Assem : Assem.T with module Temp = Temp
    module Color : Color.T with module Temp = Temp
    val alloc : Assem.instr list * Frame.frame -> Assem.instr list * Color.allocation
end with module Temp = Frame.Temp and module Assem = Frame.Assem