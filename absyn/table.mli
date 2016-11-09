module type IntMapTableKey = 
sig
	type t
	val getInt: t -> int
end

module IntMapTable : functor (Key: IntMapTableKey) ->
sig
   type key
   type 'a table
   val empty : 'a table
   val enter : 'a table * key * 'a -> 'a table
   val look  : 'a table * key -> 'a option
end

