(* Problem 5. Reverse a list. *)

let rev = function 
  | [] -> []
  | a :: b -> List.append (rev b) a
