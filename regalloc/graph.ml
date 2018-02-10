open Core

type node' = int
type temp = Temp.temp

type noderep = {succ: node' list; pred: node' list}

let emptyNode = {succ=[] ;pred=[]}

let bogusNode = {succ=[ lnot 1 ]; pred=[]}

let isBogus = function
| {succ = (n::_); _} when n = lnot 1 -> True
| _ -> False

(*struct open Array
          type elem = noderep
          type vector = noderep vector
          type array = noderep array
                            end*)

(*DynamicArrayFn()*)

module A = struct

  type array
  let array (i, elem) = ()

end

type graph = noderep list

type node = graph * node'

let eq((_, a),(_, b)) = a = b

let augment (g: graph) (n: node') : node = (g, n)

let newGraph () = A.array(0, bogusNode)

let nodes g = 
  (*let b = A.bound g in*)
  let f i = if isBogus(A.sub (g, i)) then [] else (g, i)::(f (i + 1)) in 
  f 0			     

let succ(g, i) = 
  let {succ; _} = A.sub(g, i) in
  List.map ~f:(augment g) succ

let pred(g, i) = 
  let {pred; _} = A.sub(g,i) in 
  List.map (augment g) pred 

let adj gi = (pred gi) @ (succ gi)

let newNode g = (* binary search for unused node *)
  let rec look(lo,hi) =
    (* i < lo indicates i in use
    i >= hi indicates i not in use *)
    if lo = hi then 
      (A.update(g,lo,emptyNode); (g,lo))
    else 
      let m = (lo + hi) / 2 in 
      if isBogus(A.sub(g,m)) then 
        look(lo,m) 
      else 
        look(m+1,hi)
  in
  look(0, 1 + A.bound g)

exception GraphEdge

let check(g, g') = (* if g=g' then () else raise GraphEdge *) ()

let rec delete = function 
  | (i, j::rest) -> if i = j then rest else j::delete(i,rest)
  | (_, []) -> raise GraphEdge

let diddle_edge change {f = ((g : graph), i); t =((g': graph), j)} = 
  check(g, g');
  let {succ = si; pred = pi} = A.sub(g, i) in
  A.update(g, i, {succ = change(j,si); pred = pi});
  let {succ = sj; pred = pj} = A.sub(g, j);
  A.update(g, j, {succ = sj; pred = change(i, pj)});
  ()

let mk_edge = diddle_edge (fun (a,b) -> a::b)
let rm_edge = diddle_edge delete

module NodeKey = struct
      type t = node
      let getInt (g,n) = n
end

type Table = IntMapTable(NodeKey)

let nodename(g, i) = "n" ^ Int.toString(i)
