type depth = int
type escEnv = (depth * bool ref) Symbol.table
val findEscape: Absyn.exp -> escEnv