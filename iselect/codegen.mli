module F : functor(Frame: Frame.T) -> sig

module Assem = Assem.F(Frame.Temp)
module Tree = Frane.Tree
val codegen: Frame.frame -> Tree.stm -> Assem.instr list

end