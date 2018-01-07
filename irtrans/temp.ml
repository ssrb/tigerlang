type temp = int
type label = Symbol.symbol

let temps = ref 100
let newtemp () = let t = !temps in begin temps := t + 1; t end
let makestring t = "t" ^ (string_of_int t)
let newlabel () = Symbol.symbol ""
let namedlabel = Symbol.symbol

(*structure Table = IntMapTable(type key = int
				  fun getInt n = n)

local structure F = Format
      fun postinc x = let val i = !x in x := i+1; i end
      val labs = ref 0
in
    fun newlabel() = Symbol.symbol(F.format "L%d" [F.INT(postinc labs)])
end*)
