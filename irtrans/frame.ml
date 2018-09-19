module type T = sig

module Temp : Temp.T
module Tree : Tree.T with module Temp = Temp
module Assem : Assem.T with module Temp = Temp 

type frame [@@deriving sexp]
type access  [@@deriving sexp]
type frag = PROC of {body: Tree.stm; frame: frame} | STRING of Temp.label * string [@@deriving sexp]

type register = string [@@deriving sexp]
type regclass = string [@@deriving sexp]
type targetmodel = { 
    regs: register list; 
    conflict: register -> register -> bool; 
    classes: regclass -> Core.String.Set.t; 
    colorable: Assem.Variable.t -> Assem.Variable.t list -> bool 
}

val targetmodel: targetmodel

val fp: Temp.temp
val rv: Temp.temp
val wordSize: int
val calleesaves: Assem.Variable.t list
val callersaves: Assem.Variable.t list

val externalCall: string * Tree.exp list -> Tree.exp

val newFrame: name:Temp.label -> formals:bool list -> frame
val name: frame -> Temp.label
val formals: frame -> access list
val allocLocal: frame -> bool -> access
val exp: access * Tree.exp * bool -> Tree.exp
val procEntryExit1: frame * Tree.stm -> Tree.stm
val procEntryExit2: frame * Assem.instr list -> Assem.instr list
type procEntryExit3 = {prolog: string; body: Assem.instr list; epilog: string} [@@deriving sexp]
val procEntryExit3: frame * Assem.instr list -> procEntryExit3
val tempMap: register Temp.Table.table

end
