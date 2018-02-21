module F = functor(Frame : Frame.T) ->
struct
    module Temp = Frame.Temp
    module Assem = Assem.F(Temp)
    type allocation = Frame.register Temp.Table.table
    let alloc _ = ([], Temp.Table.empty)
end