open Core.Std

type symbol = string * int [@@deriving sexp]

module H = Hashtbl

let nextsym = ref 0

let sizeHint = 128

let hashtable = String.Table.create ~size:sizeHint ()

let symbol name =
    match H.find hashtable name with
    | Some(i) -> (name, i)
    | None -> let i = !nextsym in
      begin
          nextsym := i + 1;
  	      H.set hashtable ~key:name ~data:i;
  	      (name, i)
      end

let name (s, n) = s

module SymbolKey =
struct
 type t = symbol [@@deriving sexp]
 let getInt (s, n) = n
end

module Table = Table.IntMapTable(SymbolKey)

type 'a table = 'a Table.table

let look = Table.look

let enter = Table.enter

let empty = Table.empty

