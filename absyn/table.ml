open Core

module type IntMapTableKey = 
sig
	type t [@@deriving sexp]
	val getInt: t -> int
end

module IntMapTable = functor(Key: IntMapTableKey) ->
struct
  type key = Key.t [@@deriving sexp]

  type 'a table = 'a Int.Map.t
  
  let empty = Int.Map.empty
  
  let enter (t, k, a) = Map.add_exn t ~key:(Key.getInt k) ~data:a
  
  let look (t, k) = Map.find t (Key.getInt k)

  let look_exn (t, k) = Option.value_exn ~error:(Error.create "Unknown key" k [%sexp_of: key]) (look (t, k))
end
