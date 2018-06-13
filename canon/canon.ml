module F  = functor(Tree: Tree.T) -> struct

module Temp = Temp

open Core
open Tree

let linearize stm0 =

  let (%) x y =
    match (x, y) with 
    | (EXP { t = CONST _ }, _) -> y
    | (_, EXP {t = CONST _ }) -> x
    | _ -> SEQ(x, y)
  in
  
  let commute = function
    | (EXP { t = CONST _}, _) -> true
    | (_, { t = NAME _ }) -> true
    | (_, { t = CONST _ }) -> true
    | _ -> false
  in
  
  let nop = EXP { t = CONST 0; addr = false } in (* Is it still needed ? *) 
  
  let rec reorder = function
    (*| (CALL _ as e)::rest ->
      let t = Temp.newtemp() in 
      reorder (ESEQ(MOVE(TEMP { temp = t; ptr = false }, e), TEMP { temp = t; ptr = false })::rest)*)
    | a::rest ->
      let (stms, e) = do_exp a in
      let (stms', el) = reorder rest in
      if commute(stms',e) then 
        (stms % stms',e::el)
      else 
        let t = { t = TEMP { temp = Temp.newtemp(); ptr = e.addr }; addr = e.addr }  in 
        (stms % MOVE(t, e) % stms', t::el)
    | [] -> (nop, [])

  and reorder_exp (el, build) = 
    let (stms, el') = reorder el in 
    (stms, build el')

  and reorder_stm (el, build) = 
    let (stms, el') = reorder el in 
    stms % (build el')

  and do_stm = function
    | SEQ (a, b) -> do_stm a % do_stm b
    | JUMP(e, labs) -> reorder_stm ([e], (fun [e] -> JUMP(e, labs)))
    | CJUMP (p, a, b, t, f) -> reorder_stm ([a;b], (fun [a; b] -> CJUMP(p, a, b, t, f)))
    | MOVE({ t = TEMP _ } as tmp, { t = CALL(e, el); addr }) -> reorder_stm (e::el, (fun (e::el) -> MOVE(tmp, { t = CALL (e, el); addr })))
    | MOVE({ t = TEMP _ } as tmp, b) -> reorder_stm ([b], (fun [b] -> MOVE(tmp, b)))
    | MOVE({ t = MEM e; addr }, b) -> reorder_stm([e; b], (fun [e; b] -> MOVE({ t = MEM e; addr },b)))
    | MOVE({ t = ESEQ(s, e) }, b) -> do_stm (SEQ(s, MOVE(e,b)))
    | EXP({ t = CALL(e, el); addr }) -> reorder_stm (e::el, (fun (e::el) -> EXP({ t = CALL(e, el); addr })))
    | EXP e -> reorder_stm ([e], (fun [e] -> EXP e))
    | s -> reorder_stm([], (fun [] -> s))

  and do_exp = function
    | { t = BINOP (p, a, b); addr } -> reorder_exp ([a;b], (fun [a;b] -> { t = BINOP(p, a, b); addr }))
    | { t = MEM a; addr } -> reorder_exp([a], (fun [a] -> { t = MEM(a); addr }))
    | { t = ESEQ (s, e) } ->
      let stms = do_stm s in
      let (stms', e) = do_exp e in 
      (stms % stms', e)
    | { t = CALL(e,el); addr } -> reorder_exp(e::el, fun (e::el) -> { t = CALL(e, el); addr })
    | e -> reorder_exp([], (fun [] -> e))

    (* linear gets rid of the top-level SEQ's, producing a list *)
    and linear = function
    | (SEQ (a,b), l) -> linear (a, linear (b,l))
    | (s, l) -> s::l

  in 
  (* body of linearize *)
  linear (do_stm stm0, [])
 
type block = stm list

let basicBlocks stms =
  (* Take list of statements and make basic blocks satisfying conditions
       3 and 4 above, in addition to the extra condition that 
      every block ends with a JUMP or CJUMP *)
  let done' = Temp.newlabel() in
  let rec blocks = function
    | ((LABEL _ as head)::tail, blist) ->
    begin
      let rec next = function 
        | ((JUMP _ as s)::rest, thisblock) -> endblock (rest, s::thisblock)
        | ((CJUMP _ as s)::rest, thisblock) -> endblock (rest, s::thisblock)
        | (((LABEL lab)::_) as stms, thisblock) -> next (JUMP({ t = NAME lab; addr = true }, [lab])::stms, thisblock)
        | (s::rest, thisblock) -> next (rest, s::thisblock)
        | ([], thisblock) -> next ([JUMP({ t = NAME done'; addr = true }, [done'])], thisblock)
      and endblock (stms, thisblock) = blocks(stms, (List.rev thisblock)::blist) in 
      next(tail, [head])
    end
    | ([], blist) -> List.rev blist
    | (stms, blist) -> blocks ((LABEL(Temp.newlabel()))::stms, blist)
  in (blocks(stms, []), done') 

let traceSchedule (blocks, done') =

  let enterblock table stms =
    match (stms, table) with
    | (((LABEL s)::_ as b), table) -> Symbol.enter(table, s, b)
    | (_, table) -> table
  in

  let rec splitlast = function
    | [x] -> ([], x)
    | h::t-> let (t', last) = splitlast t in (h::t', last)
  in

  let rec trace (table, (((LABEL lab)::_) as b), rest) = 
    
    let table = Symbol.enter(table, lab, []) in 
      match splitlast b with
      | (most, JUMP({ t = NAME lab }, _)) ->
      begin
        match Symbol.look(table, lab) with
        | Some(_::_ as b') -> most @ trace(table, b', rest)
        | _ -> b @ getnext(table,rest)
      end
      | (most, CJUMP(opr, x, y, t, f)) ->
      begin
        match (Symbol.look(table, t), Symbol.look(table, f)) with
        | (_, Some(_::_ as b')) -> b @ trace(table, b', rest)
        | (Some(_::_ as b'),  _) -> 
                most @ [CJUMP(notRel opr, x, y, f, t)]
                      @ trace(table, b', rest)
        | _ -> 
          let f' = Temp.newlabel() in 
          most @ [CJUMP(opr, x, y, t, f'); LABEL f'; JUMP({ t = NAME f; addr = true }, [f])] 
              @ getnext(table,rest)
      end
      | (most, JUMP _) -> b @ getnext(table,rest)

  and getnext = function
    | (table, ((LABEL lab::_) as b)::rest) ->
    begin
      match Symbol.look(table, lab) with
      | Some(_::_) -> trace(table, b, rest)
      | _ -> getnext(table, rest)
    end
    | (table, []) -> []

  in
  getnext(List.fold ~f:enterblock ~init:Symbol.empty blocks, blocks) @ [LABEL done']

end