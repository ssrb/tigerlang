open Core

module Temp = M68kTemp
module Tree = Tree.F(Temp)
module Assem = Assem.F(Temp) 

let fp = Temp.newtemp ()
let rv = Temp.newtemp ()
let wordSize = 4

type access = InFrame of int | InReg of Temp.temp  [@@deriving sexp]
type frame = {name: Temp.label; formals: access list; offset: int ref}  [@@deriving sexp]
type frag = PROC of {body: Tree.stm; frame: frame} | STRING of Temp.label * string [@@deriving sexp]
type register = string [@@deriving sexp]

let registers = []
let specialregs = []
let argregs = []
let calleesaves = []
let callersaves = []

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

let procEntryExit2 (frame, body) = 
    body @ 
    [ Assem.OPER {assem = ""; src = [fp; rv] @ calleesaves; dst = []; jump = None} ]

type procEntryExit3 = {prolog: string; body: Assem.instr list; epilog: string} [@@deriving sexp]
let procEntryExit3 (frame, body) = {prolog = ""; body; epilog = ""}

let tempMap = Temp.Table.empty