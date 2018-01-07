module F :
  functor (T : TokenCBs.T) ->
    sig
      exception SyntaxError of string
      val read : Lexing.lexbuf -> T.token
    end
