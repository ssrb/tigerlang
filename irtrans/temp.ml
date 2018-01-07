open Core.Std

type temp = int
type label = Symbol.symbol

let temps = ref 100
let labs = ref 0
let newtemp () = let t = !temps in temps := t + 1; t
let makestring t = "t" ^ (string_of_int t)
let newlabel () = let l = !labs in labs := l + 1; Symbol.symbol (sprintf "L%d" l)  
let namedlabel = Symbol.symbol