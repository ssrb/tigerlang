open Core

module Tree = Tree.F(M68kTemp)
module Temp = Tree.Temp

let fp = Temp.newtemp ()
let rv = Temp.newtemp ()
let wordSize = 4

type access = InFrame of int | InReg of Temp.temp  [@@deriving sexp]
type frame = {name: Temp.label; formals: access list; offset: int ref}  [@@deriving sexp]
type frag = PROC of {body: Tree.stm; frame: frame} | STRING of Temp.label * string

let (++) r inc = let x = !r in r := x + inc; x

let newFrame ~name ~formals = 
    let off = ref 0 in
    let formals = formals |> List.map ~f:(fun f -> InFrame (off ++ 4)) in
    {name; formals; offset = off}

let name {name; _} = name

let formals {formals; _} = formals

let allocLocal f  escape = InFrame (f.offset ++ 4)

let exp (access, exp) =
    let module T = Tree in
    match access with
    | InFrame off -> T.MEM (T.BINOP (T.PLUS, exp, (T.CONST off)))
    | InReg temp -> T.TEMP temp

let procEntryExit1 (frame, body) = body