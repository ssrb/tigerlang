module F :
 functor (Translate : Translate.T) ->
  sig
   exception Semantic_error of string
   type expty = {exp: Translate.exp; ty: Types.ty}
   val transProg : Absyn.exp -> expty
   val transProg2 : Absyn.exp -> Translate.Frame.frag list
  end