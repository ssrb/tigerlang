type graph
type node

val nodes: graph -> node list
val succ: node -> node list
val pred: node -> node list
val adj: node -> node list   (* succ + pred *)
val eq: node * node -> bool

val newGraph: unit -> graph
val newNode : graph -> node
exception GraphEdge
type edge = {f: node; t: node}
val mk_edge: edge -> unit
val rm_edge: edge -> unit

(*sharing type Table.key = node*)

val nodename: node -> string  (* for debugging only *)
