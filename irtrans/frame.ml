module type T = sig
type frame
type access
type nfparams = {name: Temp.label; formals: bool list} 
val newFrame: nfparams -> frame
val name: frame -> Temp.label
val formals: frame -> access list
val allocLocal: frame -> bool -> access
end