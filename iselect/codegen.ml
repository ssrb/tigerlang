module type T = sig
module Frame : Frame.T
module Tree = Frame.Tree
module Assem : Assem.T
val codegen: Frame.frame -> Tree.stm -> Assem.instr list
end