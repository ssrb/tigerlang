module F :
  functor (T : Tigerlex_intf.T) ->
    sig
      exception SyntaxError of string
      val read : Lexing.lexbuf -> T.token
    end
