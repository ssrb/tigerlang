type depth = int
type escEnv = (depth * bool ref) Symbol.table
fun traverseVar(env: escEnv, d: depth, s: Absyn.var): unit
fun traverseExp(env: escEnv, d: depth, s: Absyn.exp): unit
fun traverseDecs(env: escEnv, d: depth, s: Absyn.dec list): escEnv
fun findEscape(prog: Abyn.exp) : unit