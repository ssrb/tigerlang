module F : functor (Tree : Tree.T) -> sig
val printtree :  Core.Out_channel.t -> Tree.stm -> unit
end
