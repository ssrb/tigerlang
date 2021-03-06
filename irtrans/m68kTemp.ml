open Core

type temp = int [@@deriving sexp, compare]
type label = Symbol.symbol [@@deriving sexp]

let temps = ref 100
let labs = ref 0
let newtemp () = let t = !temps in temps := t + 1; t
let makestring t = "t" ^ (Int.to_string t)
let newlabel () = let l = !labs in labs := l + 1; Symbol.symbol (sprintf "L%d" l)  
let namedlabel = Symbol.symbol

module Table = struct

    module SymbolKey = struct
        type t = temp [@@deriving sexp]
        let getInt t = t
    end

    module Table = Table.IntMapTable(SymbolKey)
    type 'a table = 'a Table.table
    let empty = Table.empty
    let look  = Table.look 
    let look_exn  = Table.look_exn
    let enter = Table.enter

end

include Core.Comparable.Make (
    struct
        type t = temp [@@deriving sexp, compare] 
    end
)
