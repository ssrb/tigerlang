module F  = functor(Tree: Tree.T) -> struct

let linearize s =
  let module T = Tree in
  let (%) x y =
    match (x, y) with 
    | (T.EXP (T.CONST _), _) -> y
    | (_, T.EXP(T.CONST _)) -> x
    | _ -> T.SEQ(x,y)
  in
  let commute x y = 
    match (x, y) with
    | (T.EXP(T.CONST _), _) -> true
    | (_, T.NAME _) -> true
    | (_, T.CONST _) -> true
    | _ -> false
  in
  let nop = T.EXP(T.CONST 0) 
  in []
  (*let reorder ((e as T.CALL _ )::rest) =
	let val t = Temp.newtemp()
	 in reorder(T.ESEQ(T.MOVE(T.TEMP t, e), T.TEMP t) :: rest)
	end
    | reorder (a::rest) =
	 let val (stms,e) = do_exp a
	     val (stms',el) = reorder rest
	  in if commute(stms',e)
	     then (stms % stms',e::el)
	     else let val t = Temp.newtemp()
		   in (stms % T.MOVE(T.TEMP t, e) % stms', T.TEMP t :: el)
		  end
	 end
    | reorder nil = (nop,nil)

  and reorder_exp(el,build) = let val (stms,el') = reorder el
                        in (stms, build el')
                       end

  and reorder_stm(el,build) = let val (stms,el') = reorder (el)
		 	 in stms % build(el')
			end

  and do_stm(T.SEQ(a,b)) = 
               do_stm a % do_stm b
    | do_stm(T.JUMP(e,labs)) = 
	       reorder_stm([e],fn [e] => T.JUMP(e,labs))
    | do_stm(T.CJUMP(p,a,b,t,f)) = 
               reorder_stm([a,b], fn[a,b]=> T.CJUMP(p,a,b,t,f))
    | do_stm(T.MOVE(T.TEMP t,T.CALL(e,el))) = 
               reorder_stm(e::el,fn e::el => T.MOVE(T.TEMP t,T.CALL(e,el)))
    | do_stm(T.MOVE(T.TEMP t,b)) = 
	       reorder_stm([b],fn[b]=>T.MOVE(T.TEMP t,b))
    | do_stm(T.MOVE(T.MEM e,b)) = 
	       reorder_stm([e,b],fn[e,b]=>T.MOVE(T.MEM e,b))
    | do_stm(T.MOVE(T.ESEQ(s,e),b)) = 
	       do_stm(T.SEQ(s,T.MOVE(e,b)))
    | do_stm(T.EXP(T.CALL(e,el))) = 
	       reorder_stm(e::el,fn e::el => T.EXP(T.CALL(e,el)))
    | do_stm(T.EXP e) = 
	       reorder_stm([e],fn[e]=>T.EXP e)
    | do_stm s = reorder_stm([],fn[]=>s)

  and do_exp(T.BINOP(p,a,b)) = 
                 reorder_exp([a,b], fn[a,b]=>T.BINOP(p,a,b))
    | do_exp(T.MEM(a)) = 
		 reorder_exp([a], fn[a]=>T.MEM(a))
    | do_exp(T.ESEQ(s,e)) = 
		 let val stms = do_stm s
		     val (stms',e) = do_exp e
		  in (stms%stms',e)
		 end
    | do_exp(T.CALL(e,el)) = 
		 reorder_exp(e::el, fn e::el => T.CALL(e,el))
    | do_exp e = reorder_exp([],fn[]=>e)

  (* linear gets rid of the top-level SEQ's, producing a list *)
  fun linear(T.SEQ(a,b),l) = linear(a,linear(b,l))
    | linear(s,l) = s::l

 in (* body of linearize *)
    linear(do_stm stm0, nil)
 end*)

let basicBlocks l = ([], Tree.Temp.newlabel ())

let traceSchedule x = []

  (*type block = T.stm list

  (* Take list of statements and make basic blocks satisfying conditions
       3 and 4 above, in addition to the extra condition that 
      every block ends with a JUMP or CJUMP *)

  fun basicBlocks stms = 
     let val done = Temp.newlabel()
         fun blocks((head as T.LABEL _) :: tail, blist) =
	     let fun next((s as (T.JUMP _))::rest, thisblock) =
		                endblock(rest, s::thisblock)
		   | next((s as (T.CJUMP _))::rest, thisblock) =
                                endblock(rest,s::thisblock)
		   | next(stms as (T.LABEL lab :: _), thisblock) =
                                next(T.JUMP(T.NAME lab,[lab]) :: stms, thisblock)
		   | next(s::rest, thisblock) = next(rest, s::thisblock)
		   | next(nil, thisblock) = 
			     next([T.JUMP(T.NAME done, [done])], thisblock)
		 
		 and endblock(stms, thisblock) = 
		            blocks(stms, rev thisblock :: blist)
		     
	     in next(tail, [head])
	     end
	   | blocks(nil, blist) = rev blist
	   | blocks(stms, blist) = blocks(T.LABEL(Temp.newlabel())::stms, blist)
      in (blocks(stms,nil), done)
     end

  fun enterblock(b as (T.LABEL s :: _), table) = Symbol.enter(table,s,b)
    | enterblock(_, table) = table

  fun splitlast([x]) = (nil,x)
    | splitlast(h::t) = let val (t',last) = splitlast t in (h::t', last) end

  fun trace(table,b as (T.LABEL lab :: _),rest) = 
   let val table = Symbol.enter(table, lab, nil)
    in case splitlast b
     of (most,T.JUMP(T.NAME lab, _)) =>
	  (case Symbol.look(table, lab)
            of SOME(b' as _::_) => most @ trace(table, b', rest)
	     | _ => b @ getnext(table,rest))
      | (most,T.CJUMP(opr,x,y,t,f)) =>
          (case (Symbol.look(table,t), Symbol.look(table,f))
            of (_, SOME(b' as _::_)) => b @ trace(table, b', rest)
             | (SOME(b' as _::_), _) => 
		           most @ [T.CJUMP(T.notRel opr,x,y,f,t)]
		                @ trace(table, b', rest)
             | _ => let val f' = Temp.newlabel()
		     in most @ [T.CJUMP(opr,x,y,t,f'), 
				T.LABEL f', T.JUMP(T.NAME f,[f])]
			     @ getnext(table,rest)
                        end)
      | (most, T.JUMP _) => b @ getnext(table,rest)
     end

  and getnext(table,(b as (T.LABEL lab::_))::rest) = 
           (case Symbol.look(table, lab)
             of SOME(_::_) => trace(table,b,rest)
              | _ => getnext(table,rest))
    | getnext(table,nil) = nil

  fun traceSchedule(blocks,done) = 
       getnext(foldr enterblock Symbol.empty blocks, blocks)
         @ [T.LABEL done]*)

end