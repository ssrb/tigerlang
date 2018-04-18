open Core

module Temp = M68kTemp
module Tree = Tree.F(Temp)
module Assem = Assem.F(Temp)
module Var = Assem.Variable
module TT = Temp.Table
module SM = String.Map

let wordSize = 4

type access = InFrame of int | InReg of Temp.temp  [@@deriving sexp]
type frame = {name: Temp.label; formals: access list; offset: int ref}  [@@deriving sexp]
type frag = PROC of {body: Tree.stm; frame: frame} | STRING of Temp.label * string [@@deriving sexp]
type register = string [@@deriving sexp]
type regclass = string [@@deriving sexp]

let registers = 
    (List.init 8 ~f:(fun i -> "d" ^ Int.to_string i)) @ 
    (List.init 8 ~f:(fun i -> "a" ^ Int.to_string i)) @
    [ (* "ssp" ;*) (* "pc" ;*) (*"ccr"*) ]

type targetmodel = { regs: register list; conflict: register -> register -> bool; classes: regclass -> register list }
let targetmodel = { regs = registers; conflict = (fun _ _ -> false); classes = (fun _ -> []) }

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

(* Seb: LVO := Library Vector offset*)

let regMap, tempMap = List.fold ~init:(SM.empty, TT.empty) ~f:(fun (rmap, tmap) reg ->
    let tmp = Temp.newtemp () in
    SM.set rmap reg tmp,
    TT.enter (tmap, tmp, reg)
) registers

let fp = SM.find_exn regMap "a5"
let rv = SM.find_exn regMap "d0"
let usp = SM.find_exn regMap "a7"

let specialregs = []
let argregs = []
let calleesaves = Var.(([ "d2"; "d3"; "d4"; "d5"; "d6"; "d7" ] |> List.map ~f:(fun r -> make ((SM.find_exn regMap r), "d")))
@ (["a2"; "a3"; "a4"; (* "a5" ;*) "a6"; "a7" ] |> List.map ~f:(fun r -> make ((SM.find_exn regMap r), "a"))))
let callersaves = [ "d0"; "d1"; "a0"; "a1" ] |> List.map ~f:(SM.find_exn regMap)

let externalCall (name, exps) = 
    Tree.CALL (Tree.NAME (Temp.namedlabel ("_" ^ name)), exps)

let (--) r inc = let x = !r in r := x - inc; x

let allocLocal f escape = 
    if escape then
        InFrame (f.offset -- wordSize)
    else
        InReg (Temp.newtemp ())

let newFrame ~name ~formals =
    let f  = {name; formals = []; offset = ref 0} in
    let formals = formals |> List.map ~f:(allocLocal f) in
    {f with formals}

let name {name; _} = name

let formals {formals; _} = formals

let exp (access, exp) =
    let module T = Tree in
    match access with
    | InFrame off -> T.MEM (T.BINOP (T.PLUS, exp, (T.CONST off)))
    | InReg temp -> T.TEMP temp

let procEntryExit1 (frame, body) = 
    
    let saverestore = calleesaves |> List.map ~f:(fun (reg : Var.t) ->
        let memory = exp ((allocLocal frame false), (Tree.TEMP fp)) in
        (Tree.MOVE (memory, (Tree.TEMP reg.temp)) , Tree.MOVE ((Tree.TEMP reg.temp), memory))
    )
    in
    
    let params = frame.formals |> List.mapi ~f:(fun i f -> 
        let frame = exp (InFrame ((i + 2)* wordSize), (Tree.TEMP fp)) in
        let reg = exp (f, (Tree.TEMP fp)) in
        Tree.MOVE (reg, frame)
    ) 
    in
    
    Tree.seq ((saverestore |> List.map ~f:fst) @ params @ [ body ] @ (saverestore |> List.map ~f:snd))

let procEntryExit2 (frame, body) = 
    body @ 
    [ Assem.OPER {assem = ""; src = Var.([make (fp, "fp"); make (rv, "d") ]) @ calleesaves; dst = []; jump = None} ]

type procEntryExit3 = {prolog: string; body: Assem.instr list; epilog: string} [@@deriving sexp]
let procEntryExit3 (frame, body) = 
    let format asm = asm |> List.map ~f:(fun i -> "\t" ^ i ^ "\n") |> String.concat in
    let prolog = format [ "move fp,-(sp)"; "move sp,fp"; "suba.l #" ^ (Int.to_string !(frame.offset)) ^ ",sp" ] in
    let epilog = format [ "move fp,sp"; "move (sp)+,fp"; "rts" ] in
    { prolog; body; epilog}

