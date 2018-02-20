signature REG_ALLOCA = 
sig
    structure Frame : Frame
    type allocation = Frame.register Temp.table
    val alloc : Assem.instr list 8 Frame.frame ->
    Assem.instr list * allocation
end