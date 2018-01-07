module F :
 functor (Frame : Frame.T) ->
  sig
   exception Semantic_error of string
   type expty = {exp: Translate.exp; ty: Types.ty}
   val transProg : Absyn.exp -> expty
  end