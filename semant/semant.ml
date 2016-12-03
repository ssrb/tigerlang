open Core.Std

module A = Absyn

type venv = Env.enventry Symbol.table
type tenv = Types.ty Symbol.table

type expty = {exp: Translate.exp; ty: Types.ty}

let rec transExp (venv, tenv, exp) = 
  match exp with
  | A.VarExp v -> { exp = (); ty = Types.NIL }
  | A.NilExp -> { exp = (); ty = Types.NIL }
  | A.IntExp i -> { exp = (); ty = Types.INT }
  | A.StringExp (s,p) -> { exp = (); ty = Types.STRING }
  | A.CallExp {func;args;pos} -> { exp = (); ty = Types.NIL }
  | A.OpExp {left;oper;right;pos} ->
    let {exp = _; ty = tyleft} = transExp (venv, tenv, left)
    and {exp = _; ty = tyright} = transExp (venv, tenv, right)
    in
    if tyleft = Types.INT && tyright = Types.INT then
        { exp = (); ty = Types.INT }
    else
        (*error "integer required"*)  { exp = (); ty = Types.NIL }
  | A.RecordExp {fields;typ;pos} -> { exp = (); ty = Types.NIL } 
  | A.SeqExp l -> { exp = (); ty = Types.NIL }
  | A.AssignExp {var=v;exp=e;pos} -> { exp = (); ty = Types.NIL }
  | A.IfExp {test;then';else';pos} ->
      (match else' with
      | None -> { exp = (); ty = Types.NIL }
      | Some e -> { exp = (); ty = Types.NIL })
  | A.WhileExp {test;body;pos} -> { exp = (); ty = Types.NIL }
  | A.ForExp {var=v;escape=b;lo;hi;body;pos} -> { exp = (); ty = Types.NIL }
  | A.BreakExp p -> { exp = (); ty = Types.NIL }
  | A.LetExp {decs;body;pos} -> { exp = (); ty = Types.NIL }
  | A.ArrayExp {typ;size;init;pos} -> { exp = (); ty = Types.NIL }

let transProg e0 = transExp (Env.base_venv, Env.base_tenv, e0)

(* 
val transVar: venv * tenv * Absyn.var -> expty
val transExp: venv * tenv * Absyn.exp -> expty
val transDec: venv * tenv * Absyn.dec -> { venv: venv, tenv: tenv}
val transTy: tenv * Absyn.ty -> Types.ty




*)