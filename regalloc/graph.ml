open Core

type node' = int [@@deriving sexp]

type noderep = {succ: node' list; pred: node' list}

let emptyNode = {succ=[] ;pred=[]}

let bogusNode = {succ=[ lnot 1 ]; pred=[]}

let isBogus = function
| {succ = (n::_); _} when n = lnot 1 -> true
| _ -> false

module DynArray : sig 
  type 'a t
  exception Subscript
  val create : int * 'a -> 'a t
  val get : 'a t * int -> 'a
  val set : 'a t * int * 'a -> unit
  val bound : 'a t -> int
end = struct
  type 'a t = 'a Array.t ref * 'a * int ref
  exception Subscript
  let create (sz, dflt) = (ref (Array.create sz dflt), dflt, ref (lnot 1))
  let get ((a,  dflt,  _), idx) = 
    try !a.(idx) with Invalid_argument _ -> if idx < 0 then raise Subscript else dflt
  let expand (a, oldlen, newlen, dflt) = 
    Array.init newlen ~f:(fun i -> if i < oldlen then a.(i) else dflt)
  let set ((a, dflt, bnd), idx, v) =
    let len = Array.length !a in
    if idx >= len then 
      a := expand(!a, len, Int.max (len + len) (idx + 1), dflt);
    !a.(idx) <- v;
    if idx > !bnd then bnd := idx
  let bound (_, _, b) = !b
end

module A = DynArray

type graph = noderep A.t

type node = graph sexp_opaque * node' [@@deriving sexp]

let eq((_, a),(_, b)) = a = b

let augment (g: graph) (n: node') : node = (g, n)

let newGraph () = A.create(0, bogusNode)

let nodes g = 
  (*let b = A.bound g in*)
  let rec f i = if isBogus(A.get(g, i)) then [] else (g, i)::(f (i + 1)) in 
  f 0			     

let succ(g, i) = 
  let {succ; _} = A.get(g, i) in
  List.map ~f:(augment g) succ

let pred(g, i) = 
  let {pred; _} = A.get(g, i) in 
  List.map ~f:(augment g) pred 

let adj gi = (pred gi) @ (succ gi)

let newNode g = (* binary search for unused node *)
  let rec look(lo, hi) =
    (* i < lo indicates i in use
    i >= hi indicates i not in use *)
    if lo = hi then 
      (A.set(g, lo, emptyNode); (g, lo))
    else 
      let m = (lo + hi) / 2 in 
      if isBogus(A.get(g, m)) then 
        look(lo, m)
      else 
        look(m + 1, hi)
  in
  look(0, 1 + A.bound g)

exception GraphEdge
type edge = {f : node; t : node}

let check(g, g') = if g = g' then () else raise GraphEdge

let rec delete = function 
  | (i, j::rest) -> if i = j then rest else j::delete(i, rest)
  | (_, []) -> raise GraphEdge

let diddle_edge change ({f = ((g : graph), i); t = ((g': graph), j)} : edge) = 
  check(g, g');
  let {succ = si; pred = pi} = A.get(g, i) in
  A.set(g, i, {succ = change(j,si); pred = pi});
  let {succ = sj; pred = pj} = A.get(g, j) in
  A.set(g, j, {succ = sj; pred = change(i, pj)});
  ()

let mk_edge = diddle_edge (fun (a, b) -> a::b)
let rm_edge = diddle_edge delete

module Table = struct

  module NodeKey = struct
        type t = node
        let getInt (g, n) = n
  end

  module Table = Table.IntMapTable(NodeKey)
  type 'a table = 'a Table.table
  let empty = Table.empty
  let look  = Table.look 
  let look_exn  = Table.look_exn
  let enter = Table.enter

end

let nodename (_, i) = "n" ^ Int.to_string(i)

include Core.Comparable.Make (
  struct
    type t = node [@@deriving sexp]
    let compare (_, l) (_, r) = compare l r
  end
)
