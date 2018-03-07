open Core

module type IntMapTableKey = 
sig
	type t
	val getInt: t -> int
end

module IntMapTable = functor(Key: IntMapTableKey) ->
struct
  type key = Key.t

  type 'a table = 'a Int.Map.t
  
  let empty = Int.Map.empty
  
  let enter (t, k, a) = Map.add t ~key:(Key.getInt k) ~data:a
  
  let look (t, k) = Map.find t (Key.getInt k)

  let look_exn x = Option.value_exn (look x)
end
