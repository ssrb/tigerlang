type symbol = string * int

type H = HashTable

exception Symbol

let nextsym = ref 0

let sizeHint = 128

let hashtable : (string,int) H.hash_table = 
	H.mkTable(HashString.hashString, op = ) (sizeHint,Symbol)

let symbol name =
    match H.find hashtable name with
    | Some i => (name,i)
    | None => let i = !nextsym in
      begin
          nextsym := i+1;
  	      H.insert hashtable (name,i);
  	      (name,i)
      end

let name (s,n) = s

structure Table = IntMapTable(type key = symbol
			fun getInt(s,n) = n)

type 'a table = 'a Table.table

let empty = Table.empty

let enter = Table.enter

let look = Table.look
