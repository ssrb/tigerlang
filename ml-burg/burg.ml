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

module BurgHash = String.Table

exception BurgError				      (* for error reporting *)

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
	let rec loop pos =
		if pos <> len then  (
			f array.(pos);
			loop (pos + 1)
		)
	in
	loop 0
	
let arrayiter (f, array) =
	let len = Array.length array in
	let rec loop pos =
		if pos <> len then  (
			f (pos, array.(pos));
			loop (pos + 1)
		)
	in
	loop 0

let iter (f, n) =
	let rec loop pos =
		if pos <> n then (
			f pos;
			loop (pos + 1)
		)
	in
	loop 0
		
let listiter (f, lis) =
	let rec loop (pos, li) =
		match li with
		| [] -> ()
		| l::ll -> 
			f (pos, l); 
			loop ((pos + 1), ll)
	in
	loop (0, lis)

let explode s =
	let rec exp i l =
		if i < 0 then l else exp (i - 1) (s.[i] :: l) 
	in
	exp (String.length s - 1) []

let implode l =
	let res = String.create (List.length l) in
	let rec imp i = function
	| [] -> res
	| c :: l -> res.[i] <- c; imp (i + 1) l 
	in
	imp 0 l

exception NotSameSize

let exists2 (f, list1, list2) =
	let rec loop = function
	| ([],[]) -> false
	| (e1::l1, e2::l2) ->
		if f (e1,e2) then true else loop (l1, l2)
	| _ -> raise NotSameSize
	in
	loop (list1, list2)

let forall2 (f, l1, l2) = not (exists2 ((fun x -> not (f x)), l1, l2))

let map2 (f, list1, list2) =
	let rec loop = function
	| ([], [], acc) -> List.rev acc
	| (e1::l1, e2::l2, acc) -> loop (l1,l2,(f (e1,e2))::acc)
	| _ -> raise NotSameSize
	in
	loop (list1, list2, [])

let tofirstupper s =
	match explode s with
	| [] -> ""
	| c::r -> implode((Char.uppercase c)::(List.map r Char.lowercase))

let emit (s_in, oustreamgen) =
	(*
	* Error reporting
	*)
	let error_encountered = ref false in

	let warning s = Out_channel.(
		error_encountered := true;
		output_string stderr ("Error: " ^ s ^ "\n");
		flush stderr
	) 
	in

	let error s = Out_channel.(
		output_string stderr ("Error: " ^ s ^ "\n");
		flush stderr;
		raise BurgError
	) 
	in

	let stop_if_error () = 
		if !error_encountered then 
			raise BurgError 
	in

	(*
	* ids (for hashing) :
	* TERMINAL (internal terminal number, external terminal string/number)
	* NONTERMINAL (internal nonterminal number)
	*)
	let module F = struct
	
		type ids = TERMINAL of int * string | NONTERMINAL of int
		
		(* hash table type *)
		type htt = ids BurgHash.t
		
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
		type rule = { nt : int; pat : rule_pat; ern : ern; cost : int; num : int}

	end
	in let open F in

	(* hash table symbols *)
	let ht : htt = BurgHash.create () in

	(* hash table for rule names and the arity of the pattern *)
	let hr : int BurgHash.t = BurgHash.create () in

	(* %start symbol *)
	let start_sym = ref (None : string option) in

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
	let gen_tnum () = let t = !nb_t in nb_t := !nb_t + 1; t in

	(* Return a new internal nonterminal number *)
	let gen_ntnum () = let t = !nb_nt in nb_nt := !nb_nt + 1; t in

	(*
	* Emit the header
	*)
	let emit_header (SPEC {head; _}) = List.iter head say in

	(*
	* Emit the tail
	*)
	let emit_tail (SPEC {tail; _}) = List.iter tail say in


	(*
	* Give each terminal an internal terminal number,
	* and remember the external terminal number.
	* Also, find start symbol.
	*)
	let reparse_decls (SPEC {decls = decls; _}) =
	
		let t_prefix = ref (None : string option) in
		let r_prefix = ref (None : string option) in
		let s_name = ref (None : string option) in
			
		let newt (sym, etn') =
			let etn = match etn' with
								|	Some str -> str
								| None -> sym
			in
			match ((BurgHash.find ht sym) : ids option) with
			| None -> BurgHash.add_exn ht sym (TERMINAL (gen_tnum(), etn))
			| Some _ -> warning ("term " ^ sym ^ " redefined")
		in
			
		let newdecl = function
			| START s ->
				begin
						match !start_sym with
						| None -> start_sym := Some s
						| Some _ -> warning "%start redefined"
				end
			| TERM l -> List.iter l newt
			| TERMPREFIX tp ->
				begin
					match !t_prefix with	
					| None -> t_prefix := Some tp
					| _ -> warning "%termprefix redefined"
				end
			| RULEPREFIX rp ->
				begin
					match !r_prefix with
					| None -> r_prefix := Some rp
					| _ -> warning "%ruleprefix redefined"
				end
			| SIG s ->
				begin
					match !s_name with
					| None -> s_name := Some s
					| _ -> warning "%sig redefined"
				end
		in
		List.iter decls newdecl;

		if !nb_t = 0 then error "no terminals !";
		
		term_prefix :=
			(match !t_prefix with
			| None -> ""
			| Some tp -> tp);
		
		rule_prefix :=
			(match !r_prefix with
			| None -> ""
			| Some rp -> rp);
		
		sig_name :=
			(match !s_name with
			| None -> "BURM"
			| Some s -> String.uppercase s);

		struct_name := tofirstupper (!sig_name)
	(* reparse_decls *)
	in


	let get_id sym =
		match ((BurgHash.find ht sym) : ids option) with
		| None -> error ("symbol " ^ sym ^ " not declared")
		| Some id -> id
	in

	(*
	* Arrays that contain for each t or nt its external symbol.
	*)
	let sym_terminals = ref (Array.create 0 ("", "")) in
	let sym_nonterminals = ref (Array.create 0 "") in

	let build_num_to_sym_arrays () =
		let store ~key ~data =
			match data with
			| TERMINAL (t, etn) -> (!sym_terminals).(t) <- (key, etn)
			| NONTERMINAL nt -> (!sym_nonterminals).(nt) <- key
		in
		sym_terminals := Array.create !nb_t ("","");
		sym_nonterminals := Array.create !nb_nt "";
		BurgHash.iteri ht ~f:store
	in

	let get_ntsym nt = (!sym_nonterminals).(nt) in
	let get_tsym t = fst (!sym_terminals).(t) in

	let reparse_rules (SPEC {rules = spec_rules; _}) =

		(* Arity for terminals. *)
		let t_arity = Array.create !nb_t (None : int option) in
		let newnt (RULE (ntsym, _, _, _)) =
			match ((BurgHash.find ht ntsym) : ids option) with
			| None -> BurgHash.add_exn ht ntsym (NONTERMINAL (gen_ntnum ()))
			| Some (TERMINAL _) -> warning (ntsym ^ " redefined as a nonterminal")
			| Some (NONTERMINAL _) -> ()
		in

		(* first rule is rule 1 *)
		let rule_num = ref 0 in   
			
		let newrule (RULE (ntsym, pattern, ern, costlist)) =

			let num = (rule_num := !rule_num + 1; !rule_num) in
		
			let nt = 
				match BurgHash.find ht ntsym with
				| Some (NONTERMINAL nt) -> nt
				| _ -> error "internal : get nt"
			in
		
			let cost = match costlist with [] -> 0 | (c::_) -> c in
		
			let pat =
				let rec makepat (PAT (sym, sons)) =
					match get_id sym with
					| NONTERMINAL nt ->
						if not (List.is_empty sons) then
							warning ("nonterminal "^ sym ^ " is not a tree");
						NT nt
					| TERMINAL (t, _) ->
						let len = List.length sons in
						(match t_arity.(t) with
						| None -> t_arity.(t) <- Some len
						| Some len' -> 
							if len <> len' then
								warning ("bad arity for terminal "^sym));
						T (t, List.map ~f:makepat sons)
				in
				makepat pattern
			(* val pat *)
			in

			let patarity =
				let rec cnt n = function 
				| NT _ -> n + 1
				| T (_, pat) -> List.fold ~init:n ~f:cnt pat
				in
				cnt 0 pat
			in

			(match BurgHash.find hr ern with
			| None -> BurgHash.add_exn hr ern patarity
			| Some ar -> 
				if ar <> patarity then
					warning ("rulename " ^ ern ^ " is used with patterns of different arity"));
		
			{nt=nt; pat=pat; ern=ern; cost=cost; num=num}

		(* fun newrule *)
		in

		List.iter spec_rules newnt;
		stop_if_error ();
		if !nb_nt = 0 then error "no rules !";
		let rules = Array.of_list (List.map spec_rules newrule) in
		stop_if_error ();
		build_num_to_sym_arrays ();
		let arity = Array.init !nb_t     (* terminals numbers begin at 0 *)
			(fun i -> match t_arity.(i) with
				| None ->
					warning ("terminal "^(get_tsym i)^" unused");
					0
				| Some len -> len)
		in
		stop_if_error ();
		(rules, arity)
	(* fun reparse_rules *)
	in

	let print_intarray array =
		let printit (pos, n) =
			if pos > 0 then say ",";
			say (Int.to_string n)
		in
			arrayiter (printit, array)
	in

	(*
	* Print a rule.
	*)
	let print_rule ({nt; pat; ern; cost; _} : rule) =
		let rec print_sons = function 
			| [] -> ()
			| [p] -> print_pat p
			| p::pl ->
				(print_pat p; say ","; print_sons pl)
		and print_pat = function 
			| NT nt -> say (get_ntsym nt)
			| T (t, sons) ->
			begin
				say (get_tsym t);
				match List.length sons with
				| 0 -> ()
				| len -> (say "("; print_sons sons; say ")")
			end
		in
		say ((get_ntsym nt) ^ ":\t");
		print_pat pat;
		say ("\t= " ^ ern ^ " (" ^ (Int.to_string cost) ^ ");\n")
	in

	let prep_rule_cons ({ern = ern; _} : rule) = (!rule_prefix) ^ ern in

	let prep_node_cons t =
		let (sym, _) = (!sym_terminals).(t) in
		"N_" ^ sym
	in

	let prep_term_cons t = (!term_prefix) ^ (snd (!sym_terminals.(t))) in

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
		
		let rules_for_lhs = Array.create !nb_nt ([] : rule list) in
	
		let chains_for_rhs = Array.create !nb_nt ([] : rule list) in

		let add_lhs_rhs ({nt; pat; _} as rule : rule) =
			rules_for_lhs.(nt) <- rule::rules_for_lhs.(nt);
			match pat with
			| NT rhs -> chains_for_rhs.(rhs) <- rule::chains_for_rhs.(rhs)
			| _ -> ()
		in

		let findntl ({pat; _} as rule: rule) =
			let rec flat ntl = function 
				| NT nt -> nt::ntl
				| T (_,sons) -> List.fold ~init:ntl ~f:flat sons
			in
			(rule, flat [] pat)
		in

		let exception NotSamePat in

		let rec samepattern = function 
			| (NT _, NT _) -> true
			| (T (t1,spat1), T (t2, spat2)) ->
				if t1=t2
					then samepatternsons (spat1,spat2)
					else raise NotSamePat
			| _ -> raise NotSamePat
		and samepatternsons (l1, l2) =
			if (try (forall2 (samepattern, l1, l2)) with NotSameSize -> raise NotSamePat) then 
				true
			else 
				raise NotSamePat
		in

		let samepat ab = try samepattern ab with NotSamePat -> false in

		let clustersamepat rg ((({pat; _} : rule), _) as zap) =
			let rec loop = function 
			| ([], _) -> (pat,[zap])::rg
			| (((p,zapl) as e)::rest, acc) ->
				if samepat (p, pat) then 
					acc @ ((p,zap::zapl)::rest)	 (* don't keep order *)
				else 
					loop (rest, e::acc)
			in
			loop (rg, [])
		in

		let minmaxcostlhss (pat, zapl) =
			let min b (({cost; _} : rule), _) = if cost <= b then cost else b in
			let max b (({cost; _} : rule), _) = if cost >= b then cost else b in
			let mincost = List.fold ~init:inf ~f:min zapl in
			let maxcost = List.fold ~init: (lnot 1) ~f:max zapl in
			let addlhs lhss (({nt = lhs; _} : rule), _) =
				let rec loop = function 
					| ([],_) -> lhs::lhss
					| ((i::il) as e, acc) ->
						if lhs = i then 
							lhss
						else if lhs < i then 
							(List.rev acc) @ (lhs::e)
						else loop (il,i::acc)
				in
				loop (lhss, [])
			in
			let lhss = List.fold ~init:[] ~f:addlhs zapl in
			(pat, zapl, mincost, maxcost, lhss)
		in
		
		(* zapl is (rule,ntl) list *)
		let clustersamentl (pat,zapl,min,max,lhss) =
			let scan clusters (r,ntl) =
				let rec loop = function 
					| ([],_) -> ([r],ntl)::clusters
					| (((rl,ntl') as e)::rest, acc) ->
						if ntl=ntl'
						then acc@((r::rl,ntl)::rest)	 (* don't keep order *)
						else loop (rest,e::acc)
				in
				loop (clusters ,[])
			in
			let rlntll = List.fold ~init:[] ~f:scan zapl in
			(* rlntll is (rule list,ntl) list *)
			(pat,rlntll,min,max,lhss)
		in

		let module F = struct
			type utype = NotUnif | NoMG | SameG | FirstMG | SecondMG
			exception Forced of utype
		end in let open F in
		
		let rec uniftype = function 
			| (NT _, NT _) -> SameG
			| (NT _, T _) -> FirstMG
			| (T _, NT _) -> SecondMG
			| (T (t1,spat1), T (t2,spat2)) ->
				if t1 <> t2 then raise (Forced NotUnif) else (
					let sonsg = map2 (uniftype, spat1, spat2) in
					let addson b a = 
						match (a, b) with 
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
					try List.fold ~init:SameG ~f:addson sonsg with NotSameSize -> error "bug : uniftype"
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
		let clustermatches matches ((pat, _, mincost, maxcost, lhss) as elem) =
			(* works on already (increasing,unique) ordered lists *)
			let rec subset = function 
			| ([], _) -> true
			| (_, []) -> false
			| ((e1::l1) as a1, e2::l2) ->
				if e1 = e2 then subset (l1,l2)
				else if e1 > (e2 : int) then subset (a1,l2)
				else false
			in

			let module F = struct
				type sowhat = ANOTHER | NOTU | AFTER | BEFORE of int
			end in let open F in

			let rec loop = function 
			| (prev, i, []) -> prev
			| (prev, i, (p,_,min,max,lh)::rest) ->
				match unify (pat,p) with
				| NotUnif -> loop (prev,i+1,rest)
				| NoMG -> ANOTHER
				| SameG -> error "bug : clustermatches.SameG"
				| FirstMG ->
					if mincost > (max : int) && subset (lhss,lh) then
						match prev with
						| NOTU -> loop (AFTER,i+1,rest)
						| AFTER -> loop (AFTER,i+1,rest)
						| BEFORE k -> ANOTHER
						| _ -> error "bug : clustermatches.FirstMG"
					else ANOTHER
				| SecondMG ->
					if min > (maxcost : int) && subset (lh,lhss) then
						match prev with
						| NOTU -> loop (BEFORE i,i+1,rest)
						| AFTER -> loop (BEFORE i,i+1,rest)
						| BEFORE k -> ANOTHER
						| _ -> error "bug : clustermatches.SecondMG"
					else ANOTHER
			in

			let rec insertat = function 
				| (0,prev,next,e) -> (List.rev prev)@(e::next)
				| (n,prev,x::next,e) -> insertat (n-1,x::prev,next,e)
				| (_,prev,[],e) -> List.rev (e::prev)
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
					^(if List.is_empty sons then "" else
					if List.is_empty (List.tl_exn sons) then s else
						"("^s^")")
					^",_)"
					, cnt', false)
			and do_sons (sons, cnt) =
				let (s,cnt,_,iswot) = List.fold ~init:("", cnt, true, true) ~f:(fun (s,cnt,first,iswot) pat ->
					let (s',cnt',iswot') = do_pat (pat,cnt,iswot) in
					((if first then s' else s ^ "," ^ s'), cnt', false, iswot')
				) sons
				in (s, cnt, iswot)
			in

			let (string_for_match, iscst, iswot) =
				match pat with
				| T (_,sons) ->
					let (s,c,iswot) = do_sons (sons,0) in (s,c=0,iswot)
				| NT _ -> error "bug : string_for_match"
			in

			let uniqstr = let t = !uniq_cnt in uniq_cnt := !uniq_cnt + 1; Int.to_string(t) in
		
			(rlntll, string_for_match, uniqstr, iscst, iswot)
		
		in
		
		let tgroup = Array.create !nb_t ([] : rule list) in

		let addt ({pat; _} as rule: rule) =
			match pat with
			| T (t,_) -> tgroup.(t) <- rule::tgroup.(t)
			| NT _ -> ()
		in

		arrayapp (addt,rules);

		let eacht t =
			let v1 = tgroup.(t) in
			(* v1 : rule list *)
			let v2 = List.map v1 findntl in
			(* v2 : (rule * ntl) list  (= zap list) *)
			let v3 = List.fold ~init:[] ~f:clustersamepat v2 in
			(* v3 : (pattern * zap list) list *)
			let v4 = List.map v3 minmaxcostlhss in
			(* v4 : (pattern * zap list * mincost * maxcost * lhss) list*)
			let v5 = List.map v4 clustersamentl in
			(* v5 : same thing with (rule list * ntl) list  (= rlntll)
				instead of zap list *)
			let v6 = List.fold ~init:[] ~f:clustermatches v5 in
			(* v6 : (pattern * rlntll * min * max * lhss) list list *)
			(* now, inside each subgroup, compute the elements *)
			List.map v6 (fun v -> List.map v compute)
		(* : (rlntll*str_for_match*uniqstr*iscst*iswot) list list *)
		in

		let rule_groups = Array.init !nb_t eacht in
		arrayapp (add_lhs_rhs, rules);
		(rules_for_lhs, chains_for_rhs, rule_groups)
	
	in


	(*
	* Check that each nonterminal is reachable from start.
	*)
	let check_reachable (start, (rules_for_lhs : rule list array)) =
		let notseen = Array.create !nb_nt true in
		let rec explore_nt nt = (
			notseen.(nt) <- false;
			List.iter ~f:(fun ({pat; _} : rule) -> reach pat) rules_for_lhs.(nt)
		)
		and reach = function 
		| NT nt -> if notseen.(nt) then explore_nt nt
		| T (t, sons) -> List.iter ~f:reach sons
		in
		let test (nt, b) =
				if b then
					warning ("nonterminal "^(get_ntsym nt)^" is unreachable")
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
		let h : unit BurgHash.t = BurgHash.create () in
		let first = ref true in
		let onerule ({ern = ern; _} as rule : rule) =
			let name = prep_rule_cons rule in
				match BurgHash.find h name with
				| None ->
					let patarity =
					match BurgHash.find hr ern with
					|	None -> error "emit_type_rule, no rule name ?"
					| Some ar -> ar
					in
					let rec pr = function
					| 0 -> ""
					| 1 -> " of (rule * tree)"
					| n -> ((pr (n - 1))^" * (rule * tree)")
					in
					let constructor = name ^ (pr patarity) in
					BurgHash.add_exn h name ();
					if !first then first := false else say "\t\t| ";
					saynl constructor
				| Some _ -> ()
			in
			say "  datatype rule = ";
			arrayapp (onerule, rules)
		in

		let emit_ruleToString rules = 
			let h : unit BurgHash.t = BurgHash.create () in
			let first = ref true in
			let onerule ({ern; _} as rule : rule) = 
				let name = prep_rule_cons rule in
				match BurgHash.find h name with
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
					BurgHash.add_exn h name ();
					if !first then first:=false else say "      | ruleToString";
					say constructor;
					saynl (" = " ^ "\"" ^ name ^ "\"")
				| Some _ -> ()
			in
			say "    fun ruleToString ";
			arrayapp (onerule,rules)
		in

		let emit_debug rules =
			let p_nterm (i, sym) = saynl ("nonterm " ^ (Int.to_string i) ^ " : " ^ sym) in
			let p_rule (i, ({num; _} as rule : rule)) = (
				say ("rule " ^ (Int.to_string num) ^ " : ");
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
				if t <> 0 then say "\t       | ";
				saynl (prep_term_cons t)
			in
			saynl ("structure " ^ (!struct_name) ^ "Ops = struct");
			say "  datatype ops = ";
			iter (loop, !nb_t);
			saynl "end\n\n"
		in

		let emit_sig_burmgen () =
			saynl ("signature " ^ (!sig_name) ^ "_INPUT_SPEC = sig");
			saynl "  type tree";
			saynl ("  val opchildren : tree -> " ^ (!struct_name)
			^"Ops.ops * (tree list)");
			saynl "end\n\n"
		in

		let emit_sig_burm rules = 
			saynl ("signature " ^ (!sig_name) ^ " = sig");
			saynl "  exception NoMatch";
			saynl "  type tree";
			emit_type_rule rules;
			saynl "  val reduce : tree -> rule * tree";
			saynl "  val ruleToString : rule -> string";
			saynl "end\n\n"
		in

		let emit_beg_functor (rules, arity) =
			let loop_node t =
				let ar = arity.(t) in
				let rec loop_sons i =
					say "s_tree";
					if i <> ar then (say " * "; loop_sons (i+1))
				in
				say (if t=0 then "      " else "    | ");
				say (prep_node_cons t);
				if ar>0 then (say "\t\tof "; loop_sons 1);
				say "\n"
			in
			saynl ("functor " ^ (!struct_name) ^ "Gen (In : "
				^ (!sig_name) ^ "_INPUT_SPEC) : " ^ (!sig_name) ^ " =");
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
		in

		let emit_val_cst (rules, arity, chains_for_rhs, rule_groups) =
			let do_cstrule (t, (rlntll: (rule list * int list) list), uniqstr, iscst) =
				if iscst then
					let ar = arity.(t) in
					let a_cost = Array.create !nb_nt inf in
					let a_rule = Array.create !nb_nt 0 in
					let rec record (({nt=lhs; cost; num; _} : rule), c) =
						let cc = c + cost in
						if cc < a_cost.(lhs) then (
							a_cost.(lhs) <- cc;
							a_rule.(lhs) <- num;
							List.iter ~f:(fun rule -> record (rule, cc)) chains_for_rhs.(lhs)
						)
					in
					begin
						List.iter ~f:(fun (rules, _) -> List.iter ~f:(fun rule -> record (rule, 0)) rules) rlntll;
						if ar = 0 then (
							saynl ("    val leaf_" ^ (prep_node_cons t) ^ " =");
							say "      (Array.fromList [";
							print_intarray a_cost;
							say "],\n       Array.fromList [";
							print_intarray a_rule;
							saynl ("],\n       " ^ (prep_node_cons t) ^ ")")
						) else (
							say ("    val cst_cost_" ^ uniqstr ^ " = Array.fromList [");
							print_intarray a_cost;
							saynl "]";
							say ("    val cst_rule_" ^ uniqstr ^ " = Array.fromList [");
							print_intarray a_rule;
							saynl "]"
						)
					end
		in

		let do_cstrules (t, ll) =
			List.iter ~f:(List.iter ~f:(fun (rlntll,_,uniqstr,iscst,_) ->
							do_cstrule (t, rlntll, uniqstr, iscst)))  ll
		in

		let n = Int.to_string (!nb_nt) in
		let sinf = Int.to_string inf in
		arrayiter (do_cstrules, rule_groups);
		saynl ("    val s_c_nothing = Array.array ("^n^","^sinf^")");
		saynl ("    val s_r_nothing = Array.array ("^n^",0)");
		say "\n\n"
	in

	let emit_label_function (rules, arity, chains_for_rhs, rule_groups) =
		let firstcl = ref true in
		let emit_closure (nt, (rl : rule list)) =
			let firstrule = ref true in
			let emit_cl ({nt=lhs; cost; num; _} : rule) =
				let c = Int.to_string cost in
				let slhs = Int.to_string lhs in
				if !firstrule then 
					firstrule := false
				else 
					say ";\n\t   ";
				saynl ("if c + "^c^" < sub (s_c,"^slhs^") then");
				sayinl ("     (update (s_c,"^slhs^",c + "^c^");");
				sayi ("      update (s_r,"^slhs^","^(Int.to_string num)^")");
				if not (List.is_empty chains_for_rhs.(lhs)) then 
					say (";\n\t      closure_"^(get_ntsym lhs)^" (s_c, s_r, c + "^c^")");
				saynl "\n\t     )";
				sayinl "   else";
				sayi "     ()"
			in
			if not (List.is_empty rl) then (
				if !firstcl then (
					firstcl := false; 
					say "\tfun"
				) 
				else 
					say "\tand";
				saynl (" closure_"^(get_ntsym nt)^" (s_c, s_r, c) =");
				sayi "  (";
				List.iter ~f:emit_cl rl;
				saynl "\n\t  )"
			)
		in
		let nbnt = Int.to_string (!nb_nt) in
		let sinf = Int.to_string inf in
		let firstmatch = ref true in
		let emit_match t =
			(* "(" *)
			let ar = arity.(t) in
			let inlistofsons i = (
				say ("t"^(Int.to_string i));
				if i <> (ar-1) then
					say ","
			)
			in

			let listofsons () = (
				say " ("; 
				iter (inlistofsons, ar); 
				say ")"
			)
			in

			let firstcst = ref true in
			let emit_match_cst (_,str,uniq,iscst,_) =
				if iscst then
					(if !firstcst
						then (say "\t    "; firstcst := false)
						else say "\t  | ";
					saynl ("("^str^") =>");
					sayinl ("\t      (cst_cost_"^uniq^", cst_rule_"^uniq^")")
					)
			in

			let firstcase = ref true in
			let firstcaseelem = ref true in
			let emit_match_case (rlntll,str,uniq,iscst,iswot) =
				if not iscst then (

					if !firstcase then (
						firstcase := false;
						saynl "z =>";
						sayinl "\tlet";
						sayinl ("\t  val s_c = Array.array ("^nbnt^","^sinf^")");
						sayinl ("\t  val s_r = Array.array ("^nbnt^",0)");
						sayinl "\tin"
					);

					if !firstcaseelem then (
						firstcaseelem := false;
						sayinl "\tcase z of";
						sayi "\t    "
					)
					else 
						sayi "\t  | ";

					saynl ("("^str^") =>");
					sayinl "\t      (";
					
					let dorules ((rl : rule list), ntl) =
						let dorule ({nt=lhs; num; cost; _} : rule) =
							let slhs = Int.to_string lhs in
							let c = Int.to_string cost in
							sayinl ("\t\t   if c + "^c^" < sub (s_c,"^slhs^") then");
							sayinl ("\t\t     (update (s_c, "^slhs^", c + "^c^");");
							sayinl ("\t\t      update (s_r, "^slhs^", "^(Int.to_string num)^");");
						
							if not (List.is_empty chains_for_rhs.(lhs)) then
								sayinl ("\t\t      closure_"^(get_ntsym lhs)^" (s_c, s_r, c + "^c^");");

							sayinl "\t\t     ())";
							sayinl "\t\t   else ();"
						in

						sayi "\t       if ";
						
						listiter ((fun (i, nt) -> (
							if i <> 0 then say "andalso ";
							say ("sub (s"^(Int.to_string i)^"_r,"^(Int.to_string (nt:int))^")<>0 "))), ntl);
						
						saynl "then";
						sayinl "\t\t let";
						sayi ("\t\t   val c = ");

						listiter ((fun (i, nt) -> (
							if i <> 0 then say " + ";
							say ("sub (s"^(Int.to_string i)^"_c,"^(Int.to_string (nt:int))^")"))), ntl);
						
						saynl "\n\t\t\t in";
						List.iter ~f:dorule rl;
						sayinl "\t\t   ()";
						sayinl "\t\t end";
						sayinl "\t       else ();"
					in
					List.iter ~f:dorules rlntll;
					sayinl "\t       ()";
					sayinl "\t      )"
				) (* fun emit_match_case *)
			in
			(* ")(" fun emit_match *)
			if !firstmatch then (
				sayi "  "; 
				firstmatch := false
			)
			else 
				sayi "| ";
			
			say ((!struct_name)^"Ops.");
			saynl ((prep_term_cons t)^" =>");

			if ar=0 then (* leaf term *)
				if List.is_empty rule_groups.(t) then 
					sayinl ("    (s_c_nothing, s_r_nothing, " ^(prep_node_cons t)^")")
				else sayinl ("    leaf_"^(prep_node_cons t))
			else (* ar<>0 *)
				let group = rule_groups.(t) in
				let dosamecase eleml = (
					firstcaseelem := true;
					List.iter ~f:emit_match_case eleml;
					if (not (!firstcaseelem) && not (List.exists ~f:(fun (_,_,_,_,iswot) -> iswot) eleml)) then 
						sayinl "\t  | _ => ()";
					if (not (!firstcaseelem)) then 
						sayinl "\t  ;"
				)
				in
				sayinl "    let";
				sayi "      val [";
				iter (inlistofsons, ar);
				saynl "] = map rec_label children";
				sayinl "    in";
				
				if List.is_empty group then (		   
					(* transfert rule *)
					sayi "      (s_c_nothing, s_r_nothing, ";
					say (prep_node_cons t);
					listofsons ();
					saynl ")"
				) else (
				
					sayi "      let val (s_c, s_r) = case";
					listofsons ();
					saynl " of";
					List.iter ~f:(List.iter ~f:emit_match_cst) group;
					sayi (if !firstcst then "\t    " else "\t  | ");
					List.iter ~f:dosamecase group;
					
					if !firstcase then
						saynl "_ => (s_c_nothing, s_r_nothing)"
					else (
						sayinl "\t  (s_c, s_r)";
						sayinl "\tend"
					);
					
					sayi "      in (s_c, s_r, ";
					say (prep_node_cons t);
					listofsons ();
					saynl ") end"
				);

				sayinl "    end"
		in
		(* ")" fun emit_match *)
		saynl "    fun rec_label (tree : In.tree) =";
		saynl "      let";
		arrayiter (emit_closure, chains_for_rhs);
		sayinl "val (term, children) = In.opchildren tree";
		sayinl "val (s_c, s_r, t) = case term of";
		iter (emit_match, !nb_t);
		saynl "      in";
		saynl "        (s_c, s_r, t, tree)";
		saynl "      end\n"
	in

	let emit_reduce_function (rules) =
		let firstmatch = ref true in
		let domatch (({num; pat; _} as rule): rule) =
			let rec flatsons (the_sons, cnt, ntl) =
				List.fold ~init: (true, cnt, ntl, "") ~f:(fun (b, c, l, ss) patson ->
					let (c', l', ss') = flat (patson, c, l) in 
					(false, c', l', (if b then ss' else ss^","^ss'))
				) the_sons
			and flat (pat, cnt, ntl) =
				match pat with
				| NT nt -> (cnt+1, nt::ntl, "t"^(Int.to_string cnt))
				| T (t, sons) ->
					let len = List.length sons in
					let (_, cnt', ntl', s') = flatsons (sons, cnt, ntl) in
					let nexts = "(_,_,"^(prep_node_cons t)^(
						if len=0 then 
							"" 
						else (
							if len=1 then 
								" "^s' 
							else 
							" ("^s'^")")
					) ^",_)"
					in
					(cnt', ntl', nexts)
				in
				let (cnt, ntl, s) = flat (pat, 0, []) in
				let ntl = List.rev ntl in
				
				if !firstmatch then (
					firstmatch := false; 
					say "\t\t("
				) 
				else
					say "\t      | (";

				saynl ((Int.to_string num)^", "^s^") =>");
				sayi ("\t  ("^(prep_rule_cons rule));
				
				match pat with
				| NT nt -> say (" (doreduce (t0,"^(Int.to_string nt)^"))")
				| T (t, _) -> (
					match List.length ntl with
					| 0 -> ()
					| _ -> (
						say " (";
						listiter ((fun (i,nt) -> (
							if i <> 0 then
								say ", ";
				
							say ("doreduce (t"^(Int.to_string i)^","^(Int.to_string nt)^")"))), ntl);
							say ")"
						)
					);
					
					saynl ")"
			in
			saynl "    fun doreduce (stree : s_tree, nt) =";
			saynl "      let";
			sayinl "val (s_c, s_r, _, tree) = stree";
			sayinl "val cost = sub (s_c, nt)";
			saynl "      in";

			sayinl ("if cost="^(Int.to_string inf)^" then");
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
		in

		let emit_end_functor (start : int) = (
			saynl "    fun reduce (tree) =";
			saynl ("      doreduce (rec_label (tree), "^(Int.to_string start)^")");
			saynl "  end\n\n"
		)
		in

		let spec = let t = Parse.parse s_in in In_channel.close s_in; t in
		reparse_decls spec;
		let (rules, arity) = reparse_rules spec in
		let start =
			match !start_sym with
			| None -> 0
			| Some sym ->
			begin
				match get_id sym with
				| TERMINAL _ -> error ("cannot start on a terminal")
				| NONTERMINAL n -> n
			end
		in
		(* rule numbers for each nonterminal (array) *)
		let (rules_for_lhs, chains_for_rhs, rule_groups) = build_rules_tables rules in
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
		Out_channel.close(!s_out)
		(* fun emit *)


