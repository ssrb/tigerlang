open Core.Std

module A = Absyn

let print outstream e0 =

 let say s =  output_string outstream s in

 let sayln s = (say s; say "\n") in

 let rec indent = function 
 	| 0 -> ()
    | i -> (say " "; indent(i-1))
 in

 let opname = function 
 	| A.PlusOp -> "PlusOp"
    | A.MinusOp -> "MinusOp"
    | A.MulOp -> "TimesOp"
    | A.DivOp -> "DivideOp"
    | A.EqOp -> "EqOp"
    | A.NeqOp -> "NeqOp"
    | A.LtOp -> "LtOp"
    | A.LeOp -> "LeOp"
    | A.GtOp -> "GtOp"
    | A.GeOp -> "GeOp"
  in

  let rec dolist d f a = 
  	match a with
  	| [] -> ()
  	| [ a ] -> (sayln ""; f(a,d+1))
    | a::r -> (sayln ""; f(a,d+1); say ","; dolist d f r)
  in

  let rec var (v,d) = 
  	match v with
  	| A.SimpleVar (s,p) -> (indent d; say "SimpleVar("; say(Symbol.name s); say ")")
    | A.FieldVar (v,s,p) -> (indent d; sayln "FieldVar("; var(v,d+1); sayln ","; indent(d+1); say(Symbol.name s); say ")")
    | A.SubscriptVar (v,e,p) -> (indent d; sayln "SubscriptVar("; var(v,d+1); sayln ","; exp(e,d+1); say ")")
   
  and exp (e, d) =
	match e with
	| A.VarExp v -> (indent d; sayln "VarExp("; var(v,d+1); say ")")
    | A.NilExp -> (indent d; say "NilExp")
    | A.IntExp i -> (indent d; say "IntExp("; say(Int.to_string i); say ")")
    | A.StringExp (s,p) -> (indent d; say "StringExp(\""; say s; say "\")")
    | A.CallExp {func;args;pos} -> (indent d; say "CallExp("; say(Symbol.name func); say ",["; dolist d exp args; say "])")
    | A.OpExp {left;oper;right;pos} -> (indent d; say "OpExp("; say(opname oper); sayln ","; exp(left,d+1); sayln ","; exp(right,d+1); say ")")
    | A.RecordExp {fields;typ;pos} ->
	    let f((name,e,pos),d) = 
			(indent d; say "("; say(Symbol.name name);
			 sayln ","; exp(e,d+1);
			 say ")")
	     in (indent d; say "RecordExp("; say(Symbol.name typ); sayln ",["; dolist d f fields; say "])" )
    | A.SeqExp l -> (indent d; say "SeqExp["; (*dolist d exp (map #1 l);*) say "]")
    | A.AssignExp {var=v;exp=e;pos} -> (indent d; sayln "AssignExp("; var(v,d+1); sayln ","; exp(e,d+1); say ")")
    | A.IfExp {test;then';else';pos} -> (indent d; sayln "IfExp("; exp(test,d+1); sayln ",";  exp(then',d+1);
     match else' with
	 | None -> ()
	 | Some e -> (sayln ","; exp(e,d+1)); say ")")
  	| A.WhileExp {test;body;pos} -> (indent d; sayln "WhileExp("; exp(test,d+1); sayln ","; exp(body,d+1); say ")")
    | A.ForExp {var=v;escape=b;lo;hi;body;pos} ->
		(indent d; sayln "ForExp(";
		 say(Symbol.name v); say ","; say(Bool.to_string (!b)); sayln ",";
		 exp(lo,d+1); sayln ","; exp(hi,d+1); sayln ",";
		 exp(body,d+1); say ")")
    | A.BreakExp p -> (indent d; say "BreakExp")
    | A.LetExp {decs;body;pos} -> (indent d; say "LetExp(["; dolist d dec decs; sayln "],"; exp(body,d+1); say")")
    | A.ArrayExp {typ;size;init;pos} -> (indent d; say "ArrayExp("; say(Symbol.name typ); sayln ","; exp(size,d+1); sayln ","; exp(init,d+1); say ")")

  and dec(e, d) = 
	match e with    
	| A.FunctionDec l ->
	   	let field(({name;escape;typ;pos} : A.field), d) = 
			(indent d; say "("; say(Symbol.name name);
			 say ","; say(Bool.to_string(!escape)); 
			 say ","; say(Symbol.name typ); say ")")
		in
		let f(({name;params;result;body;pos} : A.fundec),d) =
		   (indent d; say "("; say (Symbol.name name); say ",[";
		    dolist d field params; sayln "],";
		    (match result with 
	    	 | None -> say "NONE"
			 | Some(s,_) -> (say "SOME("; say(Symbol.name s); say ")"));
		    sayln ","; exp(body,d+1); say ")")
	    in indent d; say "FunctionDec["; dolist d f l; say "]"
    | A.VarDec{name;escape;typ;init;pos} ->
	   (indent d; say "VarDec("; say(Symbol.name name); say ",";
	    say(Bool.to_string (!escape)); say ",";
	    (match typ with 
    	| None -> say "NONE" 
		| Some(s,p) -> (say "SOME("; say(Symbol.name s); say ")"); sayln ","; exp(init,d+1); say ")"))
    | A.TypeDec l ->
	 (let tdec(({name;ty=t;pos} : A.typedec),d) = (indent d; say"("; 
				  	    say(Symbol.name name); sayln ",";
					    ty(t,d+1); say ")")
	  in indent d; say "TypeDec["; dolist d tdec l; say "]")
   
  and ty(t, d) = 
  	match t with
	| A.NameTy(s,p) -> (indent d; say "NameTy("; say(Symbol.name s); say ")")
    | A.RecordTy l ->  
		(let f(({name;escape;typ;pos} : A.field),d) =
			(indent d; say "("; say (Symbol.name name);
		         say ","; say (Bool.to_string (!escape)); say ",";
			 say (Symbol.name typ); say ")")
	         in indent d; say "RecordTy["; dolist d f l; say "]")
    | A.ArrayTy(s,p) -> (indent d; say "ArrayTy("; say(Symbol.name s); say ")")

 in  exp(e0,0); sayln ""; flush outstream

type venv = Env.enventry Symbol.table
type tenv = Types.ty Symbol.table

module Translate = struct
 type exp = unit
end

type expty = {exp: Translate.exp; ty: Types.ty}

let transProg e0 = ()

(* 
val transVar: venv * tenv * Absyn.var -> expty
val transExp: venv * tenv * Absyn.exp -> expty
val transDec: venv * tenv * Absyn.dec -> { venv: venv, tenv: tenv}
val transTy: tenv * Absyn.ty -> Types.ty


let rec transExp (venv, tenv, exp) = 
  match exp with
  | A.VarExp v -> ()
    | A.NilExp -> ()
    | A.IntExp i -> ()
    | A.StringExp (s,p) -> ()
    | A.CallExp {func;args;pos} -> ()
    | A.OpExp {left;oper;right;pos} ->
      let {exp = _, ty = tyleft} = transExp (venv, tenv, left)
      and {exp = _, ty = tyright} = transExp (venv, tenv, right)
      in
      if tyleft == Types.INT and tyright == Types.INT then
        { exp = (), ty = Types.INT }
      else
        error pos "integer required"
    | A.RecordExp {fields;typ;pos} -> ()    
    | A.SeqExp l -> ()
    | A.AssignExp {var=v;exp=e;pos} -> ()
    | A.IfExp {test;then';else';pos} -> ();
     match else' with
      | None -> ()
      | Some e -> ()
    | A.WhileExp {test;body;pos} -> ()
    | A.ForExp {var=v;escape=b;lo;hi;body;pos} -> ()
    | A.BreakExp p -> ()
    | A.LetExp {decs;body;pos} -> ()
    | A.ArrayExp {typ;size;init;pos} -> ()

*)