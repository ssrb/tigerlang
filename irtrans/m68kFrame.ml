open Core

module Temp = M68kTemp
module Tree = Tree.F(Temp)
module Assem = Assem.F(Temp) 
module TT = Temp.Table
module SM = String.Map

let wordSize = 4

type access = InFrame of int | InReg of Temp.temp  [@@deriving sexp]
type frame = {name: Temp.label; formals: access list; offset: int ref}  [@@deriving sexp]
type frag = PROC of {body: Tree.stm; frame: frame} | STRING of Temp.label * string [@@deriving sexp]
type register = string [@@deriving sexp]

(*

Does anybody know a ABI reference for m68k ? Interested in the argument passing for a function call. 
All C language arguments are passed via the stack (for non-LVO calls). 
The return value from a C function is in D0. Registers D0/D1 and A0/A1 may be used as scratch by the called function, 
and the caller must expect them to have been trashed. 
Registers D2-D7 and A2-A7 must be preserved by the called function. (Some compilers allowed per-argument overrides, 
but they explicitly map a register to an argument)
More concrete: who owns A6 during a function call ? The caller. Is it preserved by a function call ? 
Yes, the callee must preserve it *if* it modifies it.
register A6 is LibBase, A5 is Frame Pointer (optional), and A4 is the pointer to the .data/.bss area (for some compilers)
A6 is that Library's libbase, and most of the intra-library calls in graphics.library are LVO (register) calls, so this is not surprising.
GfxBase is an LVO call.
There are three or four main calling conventions in AmigaOS depending on who you ask. I'm writing from memory so don't quote me exactly on this.
C calling convention: Slow as frozen molases and passes everything on the stack. Supports variadic argumentss but other than that is practically worthless and usually optimized to another calling convention. Returns result in D0. Should not be used with shared libraries except for variadic arguments stub calls.
LVO calling convention: LibCalls always allow D0/D1/A0/A1 to be used as scratch and are not preserved by the subroutine. The remaining registers must be preserved by the subroutine. The A6 register holds the library base always during this calling convention. May preserve globals and locals heap in A4 unless large data model is used. May have optional frame pointer in A5 used by debugger.
Standard calling convention: Just like LVO Calling convention except no library base is required. Requires a stub function written in C calling convention to implement variadic arguments as they are not supported from here.
*)

let registers = 
    (List.init 8 ~f:(fun i -> "d" ^ Int.to_string i)) @ 
    (List.init 7 ~f:(fun i -> "a" ^ Int.to_string i)) @
    [ "usp"; "ssp"; "pc"; "ccr" ]

let regMap, tempMap = List.fold ~init:(SM.empty, TT.empty) ~f:(fun (rmap, tmap) reg ->
    let tmp = Temp.newtemp () in
    SM.set rmap reg tmp,
    TT.enter (tmap, tmp, reg)
) registers

let fp = SM.find_exn regMap "a5"
let rv = SM.find_exn regMap "d0" 

let specialregs = []
let argregs = []
let calleesaves = [ "d2"; "d3"; "d4"; "d5"; "d6"; "d7" ] |> List.map ~f:(SM.find_exn regMap)
let callersaves = [ "d0"; "d1"; "a0"; "a1" ] |> List.map ~f:(SM.find_exn regMap)

let externalCall (name, exps) = 
    Tree.CALL (Tree.NAME (Temp.namedlabel ("_" ^ name)), exps)

let (++) r inc = let x = !r in r := x + inc; x

let newFrame ~name ~formals = 
    let off = ref 0 in
    let formals = formals |> List.map ~f:(fun f -> InFrame (off ++ 4)) in
    {name; formals; offset = off}

let name {name; _} = name

let formals {formals; _} = formals

let allocLocal f escape = InFrame (f.offset ++ 4)

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
let procEntryExit3 (frame, body) = { prolog = ""; body; epilog = "" }

