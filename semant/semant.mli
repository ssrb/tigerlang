include module type of Frame_intf
module F :
 functor (Frame : frame) ->
  sig
   exception Semantic_error of string
   type expty = {exp: Translate.exp; ty: Types.ty}
   val transProg : Absyn.exp -> expty
  end