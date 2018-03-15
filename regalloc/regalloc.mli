module F : functor(Codegen : Codegen.T) ->
sig
    module Frame : Frame.T
    module Assem : Assem.T
    module Color : Color.T with module Frame = Frame and module Temp = Frame.Temp
    val alloc : Assem.instr list * Frame.frame -> Assem.instr list * Color.allocation
end with module Frame = Codegen.Frame and module Assem = Codegen.Assem