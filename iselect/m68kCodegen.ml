let codegen (frame) (stm: Tree.stm) : Assem,instr list = 
let va ilist = ref (nil: A.instr list)
fun emit x = ilist := x::!ilist
fun result (gen) = let val tt = Temp.newtemp() in gen t; t end
fun munchStm
and munchExp
in munchStm stm'
rev(!ilist)
end