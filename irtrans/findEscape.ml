type depth = int
type escEnv = (depth * bool ref) Symbol.table

let traverseExp (env, d, s) = Symbol.empty
and traverseDecs (env, d, s) = Symbol.empty
and traverseVar (env, d, s) = Symbol.empty
let findEscape prog = traverseExp (Symbol.empty, 0, prog)
