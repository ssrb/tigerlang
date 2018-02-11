open Core
let print outstream ty =
 let say s =  Out_channel.output_string outstream s in

 let sayln s = (say s; say "\n") in

 let rec indent = function 
  | 0 -> ()
  | i -> (say " "; indent(i-1))
 in

 let open Types in

 let rec print' (ty, d) =
  match ty with 
  | RECORD (tys, unique) -> 
  	indent d; sayln "{";
	  List.iter ~f:(fun (sym, ty') -> print'(ty', d+1)) tys;
	  indent d; sayln "}"
  | NIL -> indent d; sayln "nil"
  | INT -> indent d; say "int"
  | STRING -> indent d; say "string"
  | ARRAY(ty', unique) -> 
    indent d; sayln "array of";
    print'(ty', d+1)
  | NAME (sym, tyoptref) -> 
   (match !tyoptref with 
   | Some ty' -> print'(ty', d+1)
   | None -> ())
  | UNIT -> indent d; sayln "unit"

 in print' (ty, 0)