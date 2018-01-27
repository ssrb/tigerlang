module F  = functor(Tree: Tree.T) -> struct

module T = Tree
module Temp = T.Temp

open Core

let linearize stm0 =

  let (%) x y =
    match (x, y) with 
    | (T.EXP (T.CONST _), _) -> y
    | (_, T.EXP(T.CONST _)) -> x
    | _ -> T.SEQ(x, y)
  in
  
  let commute = function
    | (T.EXP(T.CONST _), _) -> true
    | (_, T.NAME _) -> true
    | (_, T.CONST _) -> true
    | _ -> false
  in
  
  let nop = T.EXP (T.CONST 0) in 
  
  let rec reorder = function
    | (T.CALL _ as e)::rest ->
      let t = Temp.newtemp() in 
      reorder (T.ESEQ(T.MOVE(T.TEMP t, e), T.TEMP t)::rest)
    | a::rest ->
      let (stms, e) = do_exp a in
      let (stms', el) = reorder rest in
      if commute(stms',e) then 
        (stms % stms',e::el)
      else 
        let t = Temp.newtemp() in 
        (stms % T.MOVE(T.TEMP t, e) % stms', T.TEMP t :: el)
    | [] -> (nop, [])

  and reorder_exp (el, build) = 
    let (stms, el') = reorder el in 
    (stms, build el')

  and reorder_stm (el, build) = 
    let (stms, el') = reorder el in 
    stms % (build el')

  and do_stm = function
    | T.SEQ (a, b) -> do_stm a % do_stm b
    | T.JUMP(e, labs) -> reorder_stm ([e], (fun [e] -> T.JUMP(e, labs)))
    | T.CJUMP (p, a, b, t, f) -> reorder_stm ([a;b], (fun [a; b] -> T.CJUMP(p, a, b, t, f)))
    | T.MOVE(T.TEMP t, T.CALL(e, el)) -> reorder_stm (e::el, (fun (e::el) -> T.MOVE(T.TEMP t,T.CALL (e, el))))
    | T.MOVE(T.TEMP t, b) -> reorder_stm ([b], (fun [b] -> T.MOVE(T.TEMP t,b)))
    | T.MOVE(T.MEM e, b) -> reorder_stm([e; b], (fun [e; b] -> T.MOVE(T.MEM e,b)))
    | T.MOVE(T.ESEQ(s, e), b) -> do_stm (T.SEQ(s, T.MOVE(e,b)))
    | T.EXP(T.CALL(e, el)) -> reorder_stm (e::el, (fun (e::el) -> T.EXP(T.CALL(e, el))))
    | T.EXP e -> reorder_stm ([e], (fun [e] -> T.EXP e))
    | s -> reorder_stm([], (fun [] -> s))

  and do_exp = function
    | T.BINOP (p, a, b) -> reorder_exp ([a;b], (fun [a;b] -> T.BINOP(p, a, b)))
    | T.MEM a -> reorder_exp([a], (fun [a] -> T.MEM(a)))
    | T.ESEQ (s, e) ->
      let stms = do_stm s in
      let (stms', e) = do_exp e in 
      (stms % stms', e)
    | T.CALL(e,el) -> reorder_exp(e::el, fun (e::el) -> T.CALL(e, el))
    | e -> reorder_exp([], (fun [] -> e))

    (* linear gets rid of the top-level SEQ's, producing a list *)
    and linear = function
    | (T.SEQ (a,b), l) -> linear (a, linear (b,l))
    | (s, l) -> s::l

  in 
  (* body of linearize *)
  linear (do_stm stm0, [])
 
type block = T.stm list

let basicBlocks stms =
  (* Take list of statements and make basic blocks satisfying conditions
       3 and 4 above, in addition to the extra condition that 
      every block ends with a JUMP or CJUMP *)
  let done' = Temp.newlabel() in
  let rec blocks = function
    | ((T.LABEL _ as head)::tail, blist) ->
    begin
      let rec next = function 
        | ((T.JUMP _ as s)::rest, thisblock) -> endblock (rest, s::thisblock)
        | ((T.CJUMP _ as s)::rest, thisblock) -> endblock (rest, s::thisblock)
        | (((T.LABEL lab)::_) as stms, thisblock) -> next (T.JUMP(T.NAME lab, [lab])::stms, thisblock)
        | (s::rest, thisblock) -> next (rest, s::thisblock)
        | ([], thisblock) -> next ([T.JUMP(T.NAME done', [done'])], thisblock)
      and endblock (stms, thisblock) = blocks(stms, (List.rev thisblock)::blist) in 
      next(tail, [head])
    end
    | ([], blist) -> List.rev blist
    | (stms, blist) -> blocks ((T.LABEL(Temp.newlabel()))::stms, blist)
  in (blocks(stms, []), done') 

let traceSchedule (blocks, done') =

  let enterblock table stms =
    match (stms, table) with
    | (((T.LABEL s)::_ as b), table) -> Symbol.enter(table, s, b)
    | (_, table) -> table
  in

  let rec splitlast = function
    | [x] -> ([], x)
    | h::t-> let (t', last) = splitlast t in (h::t', last)
  in

  let rec trace (table, (((T.LABEL lab)::_) as b), rest) = 
    
    let table = Symbol.enter(table, lab, []) in 
      match splitlast b with
      | (most, T.JUMP(T.NAME lab, _)) ->
      begin
        match Symbol.look(table, lab) with
        | Some(_::_ as b') -> most @ trace(table, b', rest)
        | _ -> b @ getnext(table,rest)
      end
      | (most, T.CJUMP(opr, x, y, t, f)) ->
      begin
        match (Symbol.look(table, t), Symbol.look(table, f)) with
        | (_, Some(_::_ as b')) -> b @ trace(table, b', rest)
        | (Some(_::_ as b'),  _) -> 
                most @ [T.CJUMP(T.notRel opr, x, y, f, t)]
                      @ trace(table, b', rest)
        | _ -> 
          let f' = Temp.newlabel() in 
          most @ [T.CJUMP(opr, x, y, t, f'); T.LABEL f'; T.JUMP(T.NAME f, [f])] 
              @ getnext(table,rest)
      end
      | (most, T.JUMP _) -> b @ getnext(table,rest)

  and getnext = function
    | (table, ((T.LABEL lab::_) as b)::rest) ->
    begin
      match Symbol.look(table, lab) with
      | Some(_::_) -> trace(table, b, rest)
      | _ -> getnext(table, rest)
    end
    | (table, []) -> []

  in
  getnext(List.fold ~f:enterblock ~init:Symbol.empty blocks, blocks) @ [T.LABEL done']

end