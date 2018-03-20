module type T = sig
module Temp : Temp.T
type flowgraph = {
      control: Graph.node list; 
      def: Temp.temp list Graph.Table.table;
      use: Temp.temp list Graph.Table.table;
      ismove: bool Graph.Table.table
}

val show : Core.Out_channel.t -> flowgraph -> (Graph.node -> string) -> unit 

end

module F : functor(Temp : Temp.T) -> T with module Temp = Temp
  (* Note:  any "use" within the block is assumed to be BEFORE a "def" 
        of the same variable.  If there is a def(x) followed by use(x)
       in the same block, do not mention the use in this data structure,
       mention only the def.

     More generally:
       If there are any nonzero number of defs, mention def(x).
       If there are any nonzero number of uses BEFORE THE FIRST DEF,
           mention use(x).

     For any node in the graph,  
           Graph.Table.look(def,node) = SOME(def-list)
           Graph.Table.look(use,node) = SOME(use-list)
   *)
