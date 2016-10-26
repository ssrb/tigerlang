include module type of Tigerlex_intf
module F :
  functor (T : tokens) ->
    sig
      exception SyntaxError of string
      val read : Lexing.lexbuf -> T.token
    end
