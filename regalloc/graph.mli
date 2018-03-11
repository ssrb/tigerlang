type graph
type node [@@deriving sexp]

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

module Table : sig
    type 'a table
    val empty : 'a table
    val look  : 'a table * node -> 'a option
    val look_exn  : 'a table * node -> 'a
    val enter : 'a table * node * 'a -> 'a table
end

val nodename: node -> string  (* for debugging only *)

val compare_node: node -> node -> int

include Core.Comparable.S with type t := node