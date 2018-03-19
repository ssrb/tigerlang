module type IntMapTableKey = 
sig
	type t [@@deriving sexp]
	val getInt: t -> int
end

module IntMapTable : functor (Key: IntMapTableKey) ->
sig
   type key = Key.t [@@deriving sexp]
   type 'a table
   val empty : 'a table
   val enter : 'a table * key * 'a -> 'a table
   val look  : 'a table * key -> 'a option
   val look_exn  : 'a table * key -> 'a
end

