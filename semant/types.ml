open Core

type unique = unit ref sexp_opaque
and ty = 
| RECORD of (Symbol.symbol * ty) list * unique
| NIL
| INT
| STRING
| ARRAY of ty * unique
| NAME of Symbol.symbol * ty option ref
| UNIT [@@deriving sexp]

let type_equal left right =
  match (left, right) with
  | (NIL, RECORD _) | (RECORD _, NIL) -> true
  | (NIL, _) | (_, NIL)-> false
  | (RECORD (_, left), RECORD (_, right))
  | (ARRAY (_, left), ARRAY (_, right))-> phys_equal left right
  | _ -> left = right

let rec actual_ty ty =
  match ty with
  | NAME (sym, actual) ->
    begin
      match !actual with
      | Some ty' -> actual_ty ty'
      | None -> assert(false)
    end
  | _ -> ty

  let is_addr ty = 
    match actual_ty ty with
    | RECORD _ | NIL | STRING | ARRAY _ -> true
    | _ -> false