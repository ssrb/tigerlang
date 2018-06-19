(* burg.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * $Log$
 * Revision 1.2  2000/06/01 18:33:42  monnier
 * bring revisions from the vendor branch to the trunk
 *
 * Revision 1.1.1.8  1999/04/17 18:56:04  monnier
 * version 110.16
 *
 * Revision 1.2  1997/10/28 15:02:45  george
 *    Made compatible with new basis
 *
# Revision 1.1.1.1  1997/01/14  01:37:59  george
#   Version 109.24
#
 * Revision 1.1.1.2  1997/01/11  18:52:29  george
 *   ml-burg Version 109.24
 *
 * Revision 1.3  1996/06/03  17:48:15  jhr
 * Changes to bring ML-Burg upto new SML/NJ library.
 *
 * Revision 1.2  1996/02/26  15:02:05  george
 *    print no longer overloaded.
 *    use of makestring has been removed and replaced with Int.toString ..
 *    use of IO replaced with TextIO
 *
 * Revision 1.1.1.1  1996/01/31  16:01:24  george
 * Version 109
 * 
 *)
open Core

module type BURGEMIT = sig
  exception BurgError
  val emit : TextIO.instream * (unit -> TextIO.outstream) -> unit
end



module BurgEmit : BURGEMIT =
  struct

    (* structure HashStringKey : HASH_KEY = struct
      type hash_key = string
      val hashVal = HashString.hashString
      val sameKey = (op =) : string * string -> bool
    end
    structure BurgHash = HashTableFn (HashStringKey)
    exception NotThere;				  (* raised by BurgHash.find *)
	
    exception BurgError				      (* for error reporting *) *)

    let inf = 16383

    open BurgAST

    (* debugging *)
    let debug s = Out_channel.(
			output_string stderr s; 
			flush stderr
		)

    (* Output functions *)
    let s_out = ref Out_channel.stdout	   (* changed into the output stream *)
    let say s = Out_channel.output_string !s_out s
    let saynl s = say (s^"\n")
    let sayi s = say ("\t"^s)
    let sayinl s = say ("\t"^s^"\n")


    let arrayapp (f, array) =
      let len = Array.length array in
			let loop pos =
	  		if pos=len then 
					()
	  		else (
	    		f (Array.sub (array, pos));
					loop (pos+1)
				)
      in
			loop 0
   
    let arrayiter (f, array) =
      let len = Array.length array in
			let loop pos =
				if pos=len then
					()
				else (
					f (pos, Array.sub (array, pos));
					loop (pos+1)
				)
			in
			loop 0

    let iter (f, n) =
      let loop pos =
	  		if pos=n then () else (f pos; loop (pos+1))
      in
			loop 0
      

    let listiter (f, lis) =
      let loop (pos, li) =
	  		match li with
	    	| [] -> ()
	  		| (l::ll) -> 
					f (pos, l); 
					loop ((pos+1), ll)
      in
			loop (0, lis)

    exception NotSameSize

    let exists2 (f, list1, list2) =
      let loop = function
			| ([],[]) -> False
	  	| (e1::l1, e2::l2) ->
	      if f (e1,e2) then True else loop (l1, l2)
	  	| _ -> raise NotSameSize
      in
			loop (list1, list2)

    let forall2 (f,l1,l2) = not (exists2 (not o f, l1, l2))

    let map2 (f, list1, list2) =
      let loop = function
			| ([],[],acc) -> rev acc
	  	| (e1::l1,e2::l2,acc) -> loop (l1,l2,(f (e1,e2))::acc)
	  	| _ -> raise NotSameSize
      in
			loop (list1,list2,[])

    let tofirstupper s =
			match String.explode s with
	  	| [] -> ""
	  	| (c::r) -> implode(Char.toUpper c :: (map Char.toLower r))

    let emit (s_in, oustreamgen) =
			(*
			* Error reporting
			*)
	    let error_encountered = ref false in
			let warning s = Out_channel.(
				error_encountered := true;
				output_string stderr "Error: "^s^"\n";
				flush stderr
			) in
			let error s = Out_channel.(
				output_string stderr "Error: "^s^"\n";
		    flush stderr;
		    raise BurgError
			) in
			let stop_if_error () = 
				if !error_encountered then 
					raise BurgError 
				else 
					()
			in
			(*
			* ids (for hashing) :
			* TERMINAL (internal terminal number, external terminal string/number)
			* NONTERMINAL (internal nonterminal number)
			*)
			let module F = struct
		
				type ids = TERMINAL of int * string | NONTERMINAL of int
				
				(* hash table type *)
				type htt = ids BurgHash.hash_table
				
				(*
				* rule_pat :
				* NT (nonterminal)
				* T (terminal, sons)
				*)
				type rule_pat = NT of int | T of int * rule_pat list
				
				(*
				* rule
				*)
				type ern = string		      (* type for external rule name *)
				type rule = { bnt:int; pat:rule_pat; ern:ern; cost: int; num:int}

			end
			in 

			(* hash table symbols *)
			let ht : htt = BurgHash.mkTable (60, NotThere)  in

			(* hash table for rule names and the arity of the pattern *)
			let hr : int BurgHash.hash_table = BurgHash.mkTable (60, NotThere) in

 			(* %start symbol *)
			let start_sym = ref (NONE : string option) in

			(* nonterminal where to start *)
			let start = ref 0 in

			(* prefix for terminals *)
			let term_prefix = ref "" in  

			(* prefix for rules *)  
			let rule_prefix = ref "" in		

			(* BURM by default *) 
			let sig_name = ref "" in	 

			(* Burm (first upper, rest lower) *) 
			let struct_name = ref "" in	   

			(* current internal terminal number *)
			let nb_t = ref 0 in
			
			(* current internal nonterminal number *)
			let nb_nt = ref 0	in

			(* Return a new internal terminal number *)
			let gen_tnum () = !nb_t before (nb_t := !nb_t + 1) in

			(* Return a new internal nonterminal number *)
			let gen_ntnum () = !nb_nt before (nb_nt := !nb_nt + 1) in


			(*
			* Emit the header
			*)
			let emit_header (SPEC {head; _}) = app say head in


			(*
			* Emit the tail
			*)
			let emit_tail (SPEC {tail; _}) = app say tail in


			(*
			* Give each terminal an internal terminal number,
			* and remember the external terminal number.
			* Also, find start symbol.
			*)
			let reparse_decls (SPEC {decls=decls; _}) =
				let t_prefix = ref (None : string option) in
				let r_prefix = ref (None : string option) in
				let s_name = ref (None : string option) in
				
				let newt (sym, etn') =
						let etn = match etn' with
											|	Some str -> str
											| None -> sym
						in
						match ((BurgHash.find ht sym) : ids option) with
						| None -> BurgHash.insert ht (sym, TERMINAL (gen_tnum(), etn))
						| Some _ -> warning ("term "^sym^" redefined")
				in
				
				let newdecl = function
				| START s ->
					begin
							match !start_sym with
							| None -> start_sym := (Some s)
							| Some _ -> warning "%start redefined"
					end
				| TERM l -> app newt l
				| TERMPREFIX tp ->
					begin
						match !t_prefix with	
						| None -> t_prefix := Some tp
						| _ -> warning "%termprefix redefined"
					end
				| RULEPREFIX rp ->
					begin
						match !r_prefix with
						| None -> r_prefix := (SOME rp)
						| _ -> warning "%ruleprefix redefined"
					end
				| SIG s ->
					begin
						match !s_name with
						| None -> s_name := (SOME s)
						| _ -> warning "%sig redefined"
					end
				in
					app newdecl decls;
					if !nb_t = 0 then error "no terminals !" else ();
					term_prefix :=
						(match !t_prefix with
						| None -> ""
						| SOME tp -> tp);
					rule_prefix :=
						(match !r_prefix with
						| None -> ""
						| Some rp -> rp);
					sig_name :=
						(match !s_name with
						| None -> "BURM"
						| SOME s -> String.translate (String.str o Char.toUpper) s);
					struct_name := tofirstupper (!sig_name)
				(* fun reparse_decls *)
			in

			let get_id sym =
				match ((BurgHash.find ht sym) : ids option) with
				| None -> error ("symbol "^sym^" not declared")
				| Some id -> id
			in

			(*
			* Arrays that contain for each t or nt its external symbol.
			*)
			let sym_terminals = ref (Array.array (0,("",""))) in
			let sym_nonterminals = ref (Array.array (0,"")) in


			let build_num_to_sym_arrays () =
				let store = function
				| (sym, TERMINAL (t, etn)) -> Array.update (!sym_terminals, t, (sym, etn))
				| (sym, NONTERMINAL nt) -> Array.update (!sym_nonterminals, nt, sym)
				in
				sym_terminals := Array.array (!nb_t, ("",""));
				sym_nonterminals := Array.array (!nb_nt, (""));
				BurgHash.appi store ht
			in

			let get_ntsym nt = Array.sub (!sym_nonterminals, nt) in
			let get_tsym t = fst (Array.sub (!sym_terminals, t)) in


			let reparse_rules SPEC {rules=spec_rules; _} =
				(* Arity for terminals. *)
				let t_arity = Array.array (!nb_t, NONE : int option) in
				let newnt (RULE (ntsym, _, _, _)) =
						match ((BurgHash.find ht ntsym) : ids option) with
						| None -> BurgHash.insert ht (ntsym, NONTERMINAL (gen_ntnum ()))
						| Some (TERMINAL _) -> warning (ntsym^" redefined as a nonterminal")
						| SOME (NONTERMINAL _) -> ()
				in
				(* first rule is rule 1 *)
				let rule_num = ref 0 in   
				
				let newrule (RULE (ntsym, pattern, ern, costlist)) =
						let num = (rule_num := !rule_num+1; !rule_num) in
						let nt = 
							match BurgHash.find ht ntsym with
							| Some (NONTERMINAL nt) -> nt
							| _ -> error "internal : get nt"
						in
						let cost = match costlist with [] -> 0 | (c::_) -> c in
					
						let pat =
							let makepat (PAT (sym, sons)) =
								match get_id sym with
								| NONTERMINAL nt -> (NT nt) before
									(if (null sons) then () else
									warning ("nonterminal "^sym^" is not a tree"))
								| TERMINAL (t, _) ->
									begin
										let len = List.length sons in
										match Array.sub (t_arity, t) with
										| None -> Array.update (t_arity, t, SOME len)
										| Some len' -> 
											if len=len' then 
												() 
											else 
												warning ("bad arity for terminal "^sym);
											T (t, map makepat sons)
										end
							in
							makepat pattern
						(* val pat *)
						in

						let patarity =
							let cnt = function 
							| (NT _, n) -> n + 1
							| (T (_, pat), n) -> List.foldl cnt n pat
							in
							cnt (pat, 0)
						in

					match (BurgHash.find hr ern) with
					| None -> BurgHash.insert hr (ern, patarity)
					| Some ar -> 
						if ar = patarity then () else
						warning ("rulename "^ern^" is used with patterns of different arity");
						{nt=nt; pat=pat; ern=ern; cost=cost; num=num}
					(* fun newrule *)
				in

				app newnt spec_rules;
				stop_if_error ();
				if !nb_nt = 0 then error "no rules !" else ();
				let rules = Array.fromList (map newrule spec_rules) in
				stop_if_error ();
				build_num_to_sym_arrays ();
				let arity = Array.tabulate (!nb_t,     (* terminals numbers begin at 0 *)
					fun i -> match Array.sub (t_arity, i) with
						| None -> 0 before
						(warning ("terminal "^(get_tsym i)^" unused"))
						| Some len -> len)
				in
				stop_if_error ();
				(rules, arity)
				(* fun reparse_rules *)
			in


			let print_intarray array =
				let printit (pos, n) =
					if pos>0 then say "," else ();
					say (Int.toString n)
				in
					arrayiter (printit, array)
			in

			(*
			* Print a rule.
			*)
			let print_rule ({nt; pat; ern; cost; _} : rule) =
				let print_sons = function 
					| [] -> ()
					| [p] -> print_pat p
					| (p::pl) ->
						(print_pat p; say ","; print_sons pl)
				in
				let print_pat = function 
					| (NT nt) -> say (get_ntsym nt)
					| (T (t, sons)) ->
					begin
						say (get_tsym t);
						match List.length sons with
						| 0 -> ()
						| len -> (say "("; print_sons sons; say ")")
					end
				in
					say ((get_ntsym nt)^":\t");
					print_pat pat;
					say ("\t= "^ern^" ("^(Int.toString cost)^");\n")
			in

			let prep_rule_cons ({ern=ern; _} : rule) = (!rule_prefix) ^ ern in


			let prep_node_cons t =
				let (sym, _) = Array.sub (!sym_terminals, t) in
				"N_" ^ sym
			in


			let prep_term_cons t = (!term_prefix)^(snd (Array.sub (!sym_terminals, t))) in


			(*
			* rules_for_lhs : array with the rules for a given lhs nt
			* chains_for_rhs : array with the chain rules for a given rhs nt
			* rule_groups :
			*      (rl,ntl,str_for_match,uniqstr,iscst,iswot) list list array
			* array of, for each terminal that begin a pattern
			*   list of, for each different "case of"
			*     list of, for each pattern in "case of"
			*       (rule list * ntl) list
			*	 string for the match expression printing
			*	 unique string for constant patterns
			*       is_cst (bool: is the pattern without nonterminals)
			*       is_wot (bool: is the pattern without terminals : A(x,y,z,t))
			*)

			let build_rules_tables (rules : rule array) =
			
				let rules_for_lhs = Array.array (!nb_nt, []:rule list) in
			
				let chains_for_rhs = Array.array (!nb_nt, []:rule list) in

				let add_lhs_rhs ({nt; pat; _} as rule: rule) =
						Array.update (rules_for_lhs, nt,
							rule::(Array.sub (rules_for_lhs, nt)));
						match pat with
						| NT rhs -> Array.update (chains_for_rhs, rhs,
							rule::(Array.sub (chains_for_rhs, rhs)))
						| _ -> ()
				in

				let findntl ({pat; _} as rule: rule) =
					let flat = function 
					| (NT nt, ntl) -> nt::ntl
					| (T (_,sons), ntl) -> List.foldr flat ntl sons
					in
					(rule, flat (pat,[]))
				in

				(*local
					exception NotSamePat;*)

				let samepattern = function 
					| (NT _, NT _) -> true
					| (T (t1,spat1), T (t2, spat2)) ->
						if t1=t2
							then samepatternsons (spat1,spat2)
							else raise NotSamePat
					| _ -> raise NotSamePat
				and samepatternsons (l1, l2) =
					if (try forall2 (fun (p1,p2) -> samepattern (p1,p2), l1, l2) with NotSameSize -> raise NotSamePat)
					then true
					else raise NotSamePat
				in

				let samepat (p1,p2) = samepattern (p1,p2) handle NotSamePat => false in

	    	let clustersamepat ((({pat; _} as zap : rule), _), rg) =
	      	let rec loop = function 
					| ([],_) -> (pat,[zap])::rg
		  		| (((p,zapl))::rest as e, acc) ->
		      	if samepat (p,pat)
						then acc@((p,zap::zapl)::rest)	 (* don't keep order *)
						else loop (rest,e::acc)
	      	in
					loop (rg, [])
	      in


				let minmaxcostlhss (pat,zapl) =
					let min ((({cost; _}:rule),_), b) = if cost<=b then cost else b in
					let max ((({cost; _}:rule),_), b) = if cost>=b then cost else b in
					let mincost = List.foldl min inf zapl in
					let maxcost = List.foldl max (lnot 1) zapl in
					let addlhs ((({nt=lhs; _} : rule),_), lhss) =
						let rec loop = function 
							| ([],_) -> lhs::lhss
							| ((i::il) as e, acc) ->
								if lhs=i then lhss
								else if lhs<i then (rev acc)@(lhs::e)
								else loop (il,i::acc)
						in
						loop (lhss, [])
				in
			
				let lhss = List.foldl addlhs [] zapl in
				(pat,zapl,mincost,maxcost,lhss)
	   
		  in
	    

	    (* zapl is (rule,ntl) list *)
	    let clustersamentl (pat,zapl,min,max,lhss) =
	      let scan ((r,ntl),clusters) =
		    	let rec loop = function 
						| ([],_) -> ([r],ntl)::clusters
		      	| (((rl,ntl') as e)::rest, acc) ->
							if ntl=ntl'
							then acc@((r::rl,ntl)::rest)	 (* don't keep order *)
							else loop (rest,e::acc)
		  		in
		    	loop (clusters ,[])
		  	in
				let rlntll = List.foldl scan [] zapl in
				(* rlntll is (rule list,ntl) list *)
				(pat,rlntll,min,max,lhss)
			in


	    (* datatype utype = NotUnif | NoMG | SameG | FirstMG | SecondMG

	    local
	      exception Forced of utype *)
	    let uniftype = function 
			| (NT _, NT _) -> SameG
			| (NT _, T _) -> FirstMG
			| (T _, NT _) -> SecondMG
			| (T (t1,spat1), T (t2,spat2)) ->
		    if t1 <> t2 then raise (Forced NotUnif) else (
					let sonsg = map2 (uniftype, spat1, spat2) in
			 		let addson = function 
					| (NotUnif,_) -> raise (Forced NotUnif)
			   	| (_,NotUnif) -> raise (Forced NotUnif)
			   	| (NoMG,_) -> NoMG
			   	| (_,NoMG) -> NoMG
			   	| (SameG,x) -> x
			   	| (x,SameG) -> x
			   	| (FirstMG, FirstMG) -> FirstMG
			   	| (SecondMG, SecondMG) -> SecondMG
			   	| _ -> NoMG
		      in
					try List.foldl addson SameG sonsg with NotSameSize -> error "bug : uniftype"
				)
	    in

	    let unify (p1,p2) = try (uniftype (p1,p2)) with Forced x -> x in


	    (* "matches" is a list.  Each elem is a list of (pat,...)
	     * in increasing order of minimum cost for the rl, and with
	     * either non-unifiable patterns, or with a pattern more general
	     * than another -- but only if the more general one is second, and
	     * it has a strictly higher cost, and all lhs of rules in the more
	     * general pattern are also lhs of some rules in the less general
	     * one (that is, if the less general rule matches, we lose
	     * nothing in not seeing the more general one).
	     * That's all.
	     *)
	    let clustermatches ((pat,_,mincost,maxcost,lhss) as elem, matches) =
	  		(* works on already (increasing,unique) ordered lists *)
		    let rec subset = function 
				| ([],_) -> true
		  	| (_,[]) -> false
		  	| ((e1::l1) as a1,e2::l2) ->
		      if e1 = e2 then subset (l1,l2)
		      else if e1 > (e2 : int) then subset (a1,l2)
		      else false
				in

				(* datatype sowhat = ANOTHER | NOTU | AFTER | BEFORE of int *)

				let rec loop = function 
				| (prev, i, []) -> prev
		  	| (prev, i, (p,_,min,max,lh)::rest) ->
		      match unify (pat,p) with
					| NotUnif -> loop (prev,i+1,rest)
		      | NoMG -> ANOTHER
		      | SameG -> error "bug : clustermatches.SameG"
		      | FirstMG ->
			  		if mincost>(max:int) andalso subset (lhss,lh) then
			      	match prev with
							| NOTU -> loop (AFTER,i+1,rest)
			      	| AFTER -> loop (AFTER,i+1,rest)
			      	| BEFORE k -> ANOTHER
			      	| _ -> error "bug : clustermatches.FirstMG"
			    	else ANOTHER
		      | SecondMG ->
			  		if min>(maxcost:int) andalso subset (lh,lhss) then
			      	match prev with
							| NOTU -> loop (BEFORE i,i+1,rest)
			      	| AFTER -> loop (BEFORE i,i+1,rest)
			      	| BEFORE k -> ANOTHER
			      	| _ -> error "bug : clustermatches.SecondMG"
			    	else ANOTHER
				in

				let rec insertat = function 
				| (0,prev,next,e) -> (rev prev)@(e::next)
				| (n,prev,x::next,e) -> insertat (n-1,x::prev,next,e)
				| (_,prev,[],e) -> rev (e::prev)
				in

				let rec _try = function 
				| ([],_) -> [elem]::matches
				| (l::ll,acc) ->
					begin
						match loop (NOTU,0,l) with
						| ANOTHER -> _try (ll,l::acc)
						| NOTU -> acc@((elem::l)::ll)	 (* don't keep order *)
						| AFTER -> acc@((l@[elem])::ll)
						| BEFORE i -> acc@((insertat (i,[],l,elem))::ll)
					end
				in
				_try (matches,[])
			in

	    let uniq_cnt = ref 0 in

	    let compute (pat, rlntll, _, _, _) =
	      
				let rec do_pat = function 
				| (NT nt, cnt, iswot) ->
		      let s = Int.to_string cnt in
					("(s"^s^"_c,s"^s^"_r,_,_)", cnt+1, iswot)
		  	| (T (t,sons), cnt, _) ->
		      let (s,cnt',_) = do_sons (sons, cnt) in
					("(_,_,"^(prep_node_cons t)
					^(if null sons then "" else
			     if null (tl sons) then s else
			       "("^s^")")
			 		^",_)"
			 		, cnt', false)
				
				and do_sons (sons,cnt) =
				  let (s,cnt,_,iswot) = List.foldl (fun (pat,(s,cnt,first,iswot)) ->
			      let (s',cnt',iswot') = do_pat (pat,cnt,iswot) in
				 		(if first then s' else s^","^s', cnt', false, iswot')
			    ) ("",cnt,true,true) sons
					in (s,cnt,iswot)
				in

				let (string_for_match, iscst, iswot) =
		  		match pat with
		    	| T (_,sons) ->
		      	let (s,c,iswot) = do_sons (sons,0) in (s,c=0,iswot)
		  		| NT _ -> error "bug : string_for_match"
				in

				let uniqstr = Int.to_string(!uniq_cnt) before (uniq_cnt := !uniq_cnt+1) in
		  
				(rlntll, string_for_match, uniqstr, iscst, iswot)
	    
			in
		  
	    let tgroup = Array.array (!nb_t, []:rule list) in

	    let addt ({pat; _} as rule: rule) =
	    	match pat with
				| T (t,_) -> Array.update (tgroup, t, rule::(Array.sub (tgroup, t)))
	      | NT _ -> ()
	    in

			arrayapp (addt,rules);

	    let eacht t =
	      let v1 = Array.sub (tgroup, t) in
				(* v1 : rule list *)
				let v2 = map findntl v1 in
				(* v2 : (rule * ntl) list  (= zap list) *)
				let v3 = List.foldl clustersamepat [] v2 in
				(* v3 : (pattern * zap list) list *)
				let v4 = map minmaxcostlhss v3 in
				(* v4 : (pattern * zap list * mincost * maxcost * lhss) list*)
				let v5 = map clustersamentl v4 in
				(* v5 : same thing with (rule list * ntl) list  (= rlntll)
					instead of zap list *)
				let v6 = List.foldl clustermatches [] v5 in
				(* v6 : (pattern * rlntll * min * max * lhss) list list *)
				(* now, inside each subgroup, compute the elements *)
				map (map compute) v6
			(* : (rlntll*str_for_match*uniqstr*iscst*iswot) list list *)
	    in

	    let rule_groups = Array.tabulate (!nb_t, eacht) in
	    arrayapp (add_lhs_rhs, rules);
	    (rules_for_lhs, chains_for_rhs, rule_groups)
	  
		in


		(*
		* Check that each nonterminal is reachable from start.
		*)
		let check_reachable (start, (rules_for_lhs : rule list array)) =
			let notseen = Array.array (!nb_nt, true) in
			let rec explore_nt nt = (
				Array.update (notseen, nt, false);
				app (fun ({pat; _}:rule) -> reach pat)
						(Array.sub (rules_for_lhs, nt))
			)
			and reach = function 
			| (NT nt) -> if Array.sub (notseen, nt) then explore_nt nt else ()
			| (T (t, sons)) -> app reach sons
			in
			let test (nt, b) =
					if b then
						warning ("nonterminal "^(get_ntsym nt)^" is unreachable")
					else 
						()
			in
			explore_nt start;
			arrayiter (test, notseen);
			stop_if_error ()
		in
	      

		(**
		** Emit the code
		**)

		let emit_type_rule rules =
	  	(* I just want a map, really, not a hashtable. *)
			let h : unit BurgHash.hash_table = BurgHash.mkTable (32, NotThere) in
	    let first = ref true in
	    let onerule ({ern = ern; _} as rule : rule) =
	      let name = prep_rule_cons rule in
					match (BurgHash.find h name) with
					| None ->
		    		let patarity =
						match (BurgHash.find hr ern) with
						|	None -> error "emit_type_rule, no rule name ?"
						| Some ar -> ar
						in
						let pr = function
						| 0 -> ""
						| 1 -> " of (rule * tree)"
						| n -> ((pr (n-1))^" * (rule * tree)")
		      	in
						let constructor = name ^ (pr patarity) in
		      	BurgHash.insert h (name, ());
		      	if !first then first := false else say "\t\t| ";
		      	saynl constructor
					| Some _ -> ()
	    	in
	    	say "  datatype rule = ";
	    	arrayapp (onerule, rules)
	  	in

      let emit_ruleToString rules = 
				let H : unit BurgHash.hash_table = BurgHash.mkTable(32,NotThere) in
	    	let first = ref true in
	    	let onerule ({ern; _} as rule : rule) = 
					let name = prep_rule_cons rule in
					match (BurgHash.find H name) with
					| None -> 
						let patarity = 
							match BurgHash.find hr ern with
							| None -> error "emit_ruleToString.onerule"
							| Some ar -> ar
		     		in
						let pr = function 
							| 0 -> ""
							| _ -> " _"
						in
		     		let constructor = "("^ name ^ (pr patarity) ^ ")" in
						BurgHash.insert H (name,());
						if !first then first:=false 
												else say "      | ruleToString";
						say constructor;
						saynl (" = " ^ "\"" ^ name ^ "\"")
	        | Some _ -> ()
      	in
	    	say "    fun ruleToString ";
	    	arrayapp (onerule,rules)
      in

			let emit_debug rules =
				let p_nterm (i, sym) = saynl ("nonterm "^(Int.toString i)^" : "^sym) in
				let p_rule (i, {num; _} as rule : rule) =
						(say ("rule "^(Int.toString num)^" : ");
						print_rule rule
						)
				in
				saynl "(***** debug info *****";
				arrayiter (p_nterm, !sym_nonterminals);
				say "\n";
				arrayiter (p_rule, rules);
				saynl "**********************)\n\n"
			in


			let emit_struct_burmterm () =
				let loop t =
					if t=0 then () else say "\t       | ";
					saynl (prep_term_cons t)
				in
				saynl ("structure "^(!struct_name)^"Ops = struct");
				say "  datatype ops = ";
				iter (loop, !nb_t);
				saynl "end\n\n"
			in

			let emit_sig_burmgen () =
				saynl ("signature "^(!sig_name)^"_INPUT_SPEC = sig");
				saynl "  type tree";
				saynl ("  val opchildren : tree -> "^(!struct_name)
				^"Ops.ops * (tree list)");
				saynl "end\n\n"
	  	in

			let emit_sig_burm rules = 
				saynl ("signature "^(!sig_name)^" = sig");
				saynl "  exception NoMatch";
				saynl "  type tree";
				emit_type_rule rules;
				saynl "  val reduce : tree -> rule * tree";
				saynl "  val ruleToString : rule -> string";
				saynl "end\n\n"
			in

			let emit_beg_functor (rules, arity) =
				let loop_node t =
					let ar = Array.sub (arity, t) in
					let loop_sons i =
						say "s_tree";
						if i=ar then () 
						else (say " * "; loop_sons (i+1))
					in
					say (if t=0 then "      " else "    | ");
					say (prep_node_cons t);
					if ar>0 then (say "\t\tof "; loop_sons 1)
					else ();
					say "\n"
				in
				saynl ("functor "^(!struct_name)^"Gen (In : "
				^(!sig_name)^"_INPUT_SPEC) : "^(!sig_name)^" =");
				saynl "  struct\n";
				saynl "    type tree = In.tree\n";
				saynl "    exception NoMatch";
				emit_type_rule rules;
				say "\n\n";
				emit_ruleToString rules; say "\n\n";
				saynl "    type s_cost = int Array.array";
				saynl "    type s_rule = int Array.array";
				saynl "    datatype s_node =";
				iter (loop_node, !nb_t);
				saynl "    withtype s_tree = s_cost * s_rule * s_node * tree\n\n";
				saynl "    val sub = Array.sub";
				saynl "    val update = Array.update"

	fun emit_val_cst (rules, arity, chains_for_rhs, rule_groups) =
	  let
	    fun do_cstrule (t, rlntll: (rule list * int list) list,
			    uniqstr, iscst) =
	      if iscst then
		let
		  val ar = Array.sub (arity, t)
		  val a_cost = Array.array (!nb_nt, inf);
		  val a_rule = Array.array (!nb_nt, 0);
		    
		  fun record ({nt=lhs, cost, num, ...} : rule, c) =
		    let
		      val cc = c + cost
		    in
		      if cc < (Array.sub (a_cost, lhs)) then
			(Array.update (a_cost, lhs, cc);
			 Array.update (a_rule, lhs, num);
			 app (fn rule => record (rule, cc))
			     (Array.sub (chains_for_rhs, lhs))
		        )
		      else ()
		    end
		in
		  app ((app (fn rule => record (rule, 0))) o #1) rlntll;
		  if ar=0 then
		    (saynl ("    val leaf_"^(prep_node_cons t)^" =");
		     say "      (Array.fromList [";
		     print_intarray a_cost;
		     say "],\n       Array.fromList [";
		     print_intarray a_rule;
		     saynl ("],\n       "^(prep_node_cons t)^")")
		    )
		  else
		    (say ("    val cst_cost_"^uniqstr^" = Array.fromList [");
		     print_intarray a_cost;
		     saynl "]";
		     say ("    val cst_rule_"^uniqstr^" = Array.fromList [");
		     print_intarray a_rule;
		     saynl "]"
		    )
		end
	      else ()

	    fun do_cstrules (t, ll) =
	      app (app (fn (rlntll,_,uniqstr,iscst,_) =>
		            do_cstrule (t, rlntll, uniqstr, iscst)))  ll
	    val n = Int.toString (!nb_nt)
	    val sinf = Int.toString inf
	  in
	    arrayiter (do_cstrules, rule_groups);
	    saynl ("    val s_c_nothing = Array.array ("^n^","^sinf^")");
	    saynl ("    val s_r_nothing = Array.array ("^n^",0)");
	    say "\n\n"
	  end


	fun emit_label_function (rules, arity, chains_for_rhs, rule_groups) =
	  let
	    val firstcl = ref true
	    fun emit_closure (nt, rl : rule list) =
	      let
		val firstrule = ref true
		fun emit_cl ({nt=lhs, cost, num, ...} : rule) =
		  let
		    val c = Int.toString cost
		    val slhs = Int.toString lhs;
		  in
		    if !firstrule
		      then firstrule := false
		      else say ";\n\t   ";
		    saynl ("if c + "^c^" < sub (s_c,"^slhs^") then");
		    sayinl ("     (update (s_c,"^slhs^",c + "^c^");");
		    sayi ("      update (s_r,"^slhs^","^(Int.toString num)
			  ^")");
		    if null (Array.sub (chains_for_rhs, lhs)) then () else
		      say (";\n\t      closure_"^(get_ntsym lhs)
			   ^" (s_c, s_r, c + "^c^")");
		      saynl "\n\t     )";
		      sayinl "   else";
		      sayi "     ()"
		  end
	      in
		if null rl then () else
		  (if !firstcl then
		     (firstcl := false; say "\tfun") else say "\tand";
		   saynl (" closure_"^(get_ntsym nt)^" (s_c, s_r, c) =");
		   sayi "  (";
		   List.app emit_cl rl;
		   saynl "\n\t  )"
		  ) 
	      end


	    val nbnt = Int.toString (!nb_nt)
	    val sinf = Int.toString inf
	    val firstmatch = ref true

	    fun emit_match t =
	      let (* "(" *)
		val ar = Array.sub (arity, t)

		fun inlistofsons i = (say ("t"^(Int.toString i));
				      if i=(ar-1) then () else say ",")

		fun listofsons () =
		  (say " ("; iter (inlistofsons, ar); say ")")

		val firstcst = ref true
		fun emit_match_cst (_,str,uniq,iscst,_) =
		  if iscst then
		    (if !firstcst
		       then (say "\t    "; firstcst := false)
		       else say "\t  | ";
		     saynl ("("^str^") =>");
		     sayinl ("\t      (cst_cost_"^uniq^", cst_rule_"^uniq^")")
		    )
		  else ()



		val firstcase = ref true
		val firstcaseelem = ref true
		fun emit_match_case (rlntll,str,uniq,iscst,iswot) =
		  if iscst then () else
		    (if !firstcase then
		       (firstcase := false;
			saynl "z =>";
			sayinl "\tlet";
			sayinl ("\t  val s_c = Array.array ("
			      ^nbnt^","^sinf^")");
			sayinl ("\t  val s_r = Array.array ("
				^nbnt^",0)");
			sayinl "\tin")
		     else ();
		     if !firstcaseelem then
		       (firstcaseelem := false;
			sayinl "\tcase z of";
			sayi "\t    ")
		     else sayi "\t  | ";
		     saynl ("("^str^") =>");
		     sayinl "\t      (";
		     let
		       fun dorules (rl : rule list, ntl) =
			 let
			   fun dorule ({nt=lhs, num, cost, ...} : rule) =
			     let
			       val slhs = Int.toString lhs
			       val c = Int.toString cost
			     in
			       sayinl ("\t\t   if c + "^c^" < sub (s_c,"^slhs
				       ^") then");
			       sayinl ("\t\t     (update (s_c, "^slhs
				       ^", c + "^c^");");
			       sayinl ("\t\t      update (s_r, "^slhs
				       ^", "^(Int.toString num)^");");
			       if null (Array.sub (chains_for_rhs, lhs)) then ()
			       else sayinl ("\t\t      closure_"
					    ^(get_ntsym lhs)
					    ^" (s_c, s_r, c + "^c^");");
			       sayinl "\t\t     ())";
			       sayinl "\t\t   else ();"
			     end
			 in
			   sayi "\t       if ";
			   listiter ((fn (i, nt) =>
				      (if i=0 then () else say "andalso ";
					 say ("sub (s"^(Int.toString i)^"_r,"
					      ^(Int.toString (nt:int))
					      ^")<>0 "))),
				     ntl);
			   saynl "then";
			   sayinl "\t\t let";
			   sayi ("\t\t   val c = ");
			   listiter ((fn (i, nt) =>
				      (if i=0 then () else say " + ";
					 say ("sub (s"^(Int.toString i)^"_c,"
					      ^(Int.toString (nt:int))^")"))),
				     ntl);
			   saynl "\n\t\t\t in";
			   app dorule rl;
			   sayinl "\t\t   ()";
			   sayinl "\t\t end";
			   sayinl "\t       else ();"
			 end
		     in
		       app dorules rlntll
		     end;
		     sayinl "\t       ()";
		     sayinl "\t      )"
		    ) (* fun emit_match_case *)

	      in (* ")(" fun emit_match *)

		if !firstmatch
		  then (sayi "  "; firstmatch := false)
		  else sayi "| ";
		say ((!struct_name)^"Ops.");
		saynl ((prep_term_cons t)^" =>");

		if ar=0 then					(* leaf term *)
		  if null (Array.sub (rule_groups, t))
		    then sayinl ("    (s_c_nothing, s_r_nothing, "
				 ^(prep_node_cons t)^")")
		    else sayinl ("    leaf_"^(prep_node_cons t))
		else						    (* ar<>0 *)
		  let
		    val group = Array.sub (rule_groups, t)
		    fun dosamecase eleml =
		      (firstcaseelem := true;
		       app emit_match_case eleml;
		       if (not (!firstcaseelem) andalso
			   not (List.exists (fn (_,_,_,_,iswot) => iswot) eleml))
			 then sayinl "\t  | _ => ()" else ();
		       if (not (!firstcaseelem)) then sayinl "\t  ;" else ()
		       )
		  in
		    sayinl "    let";
		    sayi "      val [";
		    iter (inlistofsons, ar);
		    saynl "] = map rec_label children";
		    sayinl "    in";
		    if null group then		   (* transfert rule *)
		      (sayi "      (s_c_nothing, s_r_nothing, ";
		       say (prep_node_cons t);
		       listofsons ();
		       saynl ")"
		      )
		    else
		      (sayi "      let val (s_c, s_r) = case";
		       listofsons ();
		       saynl " of";
		       app (app emit_match_cst) group;
		       sayi (if !firstcst then "\t    " else "\t  | ");
		       app dosamecase group;
		       if !firstcase then
			 saynl "_ => (s_c_nothing, s_r_nothing)"
		       else
			 (sayinl "\t  (s_c, s_r)";
			  sayinl "\tend"
			 );
		       sayi "      in (s_c, s_r, ";
		       say (prep_node_cons t);
		       listofsons ();
		       saynl ") end"
		      );
		    sayinl "    end"
		  end

	      end (* ")" fun emit_match *)


	  in
	    saynl "    fun rec_label (tree : In.tree) =";
	    saynl "      let";
	    arrayiter (emit_closure, chains_for_rhs);
	    sayinl "val (term, children) = In.opchildren tree";
	    sayinl "val (s_c, s_r, t) = case term of";
	    iter (emit_match, !nb_t);
	    saynl "      in";
	    saynl "        (s_c, s_r, t, tree)";
	    saynl "      end\n"
	  end


	fun emit_reduce_function (rules) =
	  let
	    val firstmatch = ref true

	    fun domatch (rule as {num, pat, ...} : rule) =
	      let
		fun flatsons (the_sons, cnt, ntl) =
		  List.foldl
		    (fn (patson, (b, c, l, ss)) =>
		       let
			 val (c', l', ss') = flat (patson, c, l)
		       in
			 (false, c', l', (if b then ss' else ss^","^ss'))
		       end)
		    (true, cnt, ntl, "")
		    the_sons
		and flat (pat, cnt, ntl) =
		  case pat of
		    NT nt => (cnt+1, nt::ntl, "t"^(Int.toString cnt))
		  | T (t, sons) =>
		      let
			val len = List.length sons
			val (_, cnt', ntl', s') = flatsons (sons, cnt, ntl)
			val nexts =
			  "(_,_,"^(prep_node_cons t)
			  ^(if len=0 then "" else
			      (if len=1 then " "^s' else " ("^s'^")"))
			  ^",_)"
		      in
			(cnt', ntl', nexts)
		      end

		val (cnt, ntl, s) = flat (pat, 0, [])
		val ntl = rev ntl
	      in
		if !firstmatch then (firstmatch := false; say "\t\t(") else
		  say "\t      | (";
		saynl ((Int.toString num)^", "^s^") =>");
		sayi ("\t  ("^(prep_rule_cons rule));
		case pat of
		  NT nt => say (" (doreduce (t0,"^(Int.toString nt)^"))")
		| T (t, _) =>
		    (case List.length ntl of
		       0 => ()
		     | _ =>
			 (say " (";
			  listiter ((fn (i,nt) =>
				     (if i=0 then () else say ", ";
					say ("doreduce (t"^(Int.toString i)^","
					     ^(Int.toString nt)^")"))),
				    ntl);
			  say ")")
		    );
		saynl ")"
	      end
	  in
	    saynl "    fun doreduce (stree : s_tree, nt) =";
	    saynl "      let";
	    sayinl "val (s_c, s_r, _, tree) = stree";
	    sayinl "val cost = sub (s_c, nt)";
	    saynl "      in";

sayinl ("if cost="^(Int.toString inf)^" then");
sayinl ("  (print (\"No Match on nonterminal \"^(Int.toString nt)^\"\\n\");");
sayinl ("   print \"Possibilities were :\\n\";");
sayinl ("   let");
sayinl ("     fun loop n =");
sayinl ("       let");
sayinl ("         val c = Array.sub (s_c, n);");
sayinl ("         val r = Array.sub (s_r, n);");
sayinl ("       in");
sayinl ("         if c=16383 then () else");
sayinl ("           print (\"rule \"^(Int.toString r)^\" with cost \"");
sayinl ("                  ^(Int.toString c)^\"\\n\");");
sayinl ("         loop (n+1)");
sayinl ("       end");
sayinl ("   in");
sayinl ("     (loop 0) handle General.Subscript => ()");
sayinl ("   end;");
sayinl ("   raise NoMatch)");
sayinl ("else");


	    sayinl "  let";
	    sayinl "    val rulensons =";
	    sayinl "      case (sub (s_r, nt), stree) of";
	    arrayapp (domatch, rules);
	    sayinl "      | _ => raise NoMatch (* bug in iburg *)";
	    sayinl "  in";
	    sayinl "    (rulensons, tree)";
	    sayinl "  end";
	    saynl "      end\n"
	  end
	

	fun emit_end_functor (start : int) =
	  (saynl "    fun reduce (tree) =";
	   saynl ("      doreduce (rec_label (tree), "^(Int.toString start)^")");
	   saynl "  end\n\n"
	  )

      in
	let
	  val spec = #1 (Parse.parse s_in) before TextIO.closeIn s_in
	  val _ = reparse_decls spec
	  val (rules, arity) = reparse_rules spec
	  val start =
 	    case !start_sym of
	      NONE => 0
	    | SOME sym =>
		case get_id sym of
		  TERMINAL _ => error ("cannot start on a terminal")
		| NONTERMINAL n => n
	  (* rule numbers for each nonterminal (array) *)
	  val (rules_for_lhs, chains_for_rhs, rule_groups)
	    = build_rules_tables rules
	in
	  check_reachable (start, rules_for_lhs);
	  s_out := (oustreamgen ());
	  emit_header (spec);
	  emit_debug (rules);
	  emit_struct_burmterm ();
	  emit_sig_burmgen ();
	  emit_sig_burm (rules);
	  emit_beg_functor (rules, arity);
	  emit_val_cst (rules, arity, chains_for_rhs, rule_groups);
	  emit_label_function (rules, arity, chains_for_rhs, rule_groups);
	  emit_reduce_function (rules);
	  emit_end_functor (start);
	  emit_tail (spec);
	  TextIO.closeOut (!s_out)
	end
      end (* fun emit *)

  end
