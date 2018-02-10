open Core

type node' = int

type noderep = {succ: node' list; pred: node' list}

let emptyNode = {succ=[] ;pred=[]}

let bogusNode = {succ=[ lnot 1 ]; pred=[]}

let isBogus = function
| {succ = (n::_); _} when n = lnot 1 -> true
| _ -> false

module A : sig 
  type 'a array
  val array : int * 'a -> 'a array
  val sub : 'a array * int -> 'a
  val update : 'a array * int * 'a -> unit
  val bound : 'a array -> int
end = struct
  type 'a array = 'a Array.t ref * 'a * int ref
  let array (size, dflt) = (ref (Array.create size dflt), dflt, ref (-1))
  let sub ((a, _, _), idx) = !a.(idx)
  let update ((a, _, _), idx, v) = !a.(idx) <- v
  let bound (_, _, b) = !b
end

type graph = noderep A.array

type node = graph * node'

let eq((_, a),(_, b)) = a = b

let augment (g: graph) (n: node') : node = (g, n)

let newGraph () = A.array(0, bogusNode)

let nodes g = 
  (*let b = A.bound g in*)
  let rec f i = if isBogus(A.sub (g, i)) then [] else (g, i)::(f (i + 1)) in 
  f 0			     

let succ(g, i) = 
  let {succ; _} = A.sub(g, i) in
  List.map ~f:(augment g) succ

let pred(g, i) = 
  let {pred; _} = A.sub(g, i) in 
  List.map ~f:(augment g) pred 

let adj gi = (pred gi) @ (succ gi)

let newNode g = (* binary search for unused node *)
  let rec look(lo, hi) =
    (* i < lo indicates i in use
    i >= hi indicates i not in use *)
    if lo = hi then 
      (A.update(g, lo, emptyNode); (g, lo))
    else 
      let m = (lo + hi) / 2 in 
      if isBogus(A.sub(g, m)) then 
        look(lo, m)
      else 
        look(m + 1, hi)
  in
  look(0, 1 + A.bound g)

exception GraphEdge
type edge = {f : node; t : node}

let check(g, g') = (* if g=g' then () else raise GraphEdge *) ()

let rec delete = function 
  | (i, j::rest) -> if i = j then rest else j::delete(i, rest)
  | (_, []) -> raise GraphEdge

let diddle_edge change ({f = ((g : graph), i); t = ((g': graph), j)} : edge) = 
  check(g, g');
  let {succ = si; pred = pi} = A.sub(g, i) in
  A.update(g, i, {succ = change(j,si); pred = pi});
  let {succ = sj; pred = pj} = A.sub(g, j) in
  A.update(g, j, {succ = sj; pred = change(i, pj)});
  ()

let mk_edge = diddle_edge (fun (a, b) -> a::b)
let rm_edge = diddle_edge delete

module NodeKey = struct
      type t = node
      let getInt (g, n) = n
end

module Table = Table.IntMapTable(NodeKey)

let nodename (_, i) = "n" ^ Int.to_string(i)
