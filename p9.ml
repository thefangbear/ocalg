(* Problem 9. Pack consecutive duplicates of list elements into sublists. (medium) *)

Open Printf

(* Example:
 pack ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"d";"e";"e";"e";"e"];;
- : string list list =
[["a"; "a"; "a"; "a"]; ["b"]; ["c"; "c"]; ["a"; "a"]; ["d"; "d"];
 ["e"; "e"; "e"; "e"]]
*)

let rec pack = function
  | [] -> []
  | a :: (b :: _ as t) ->
    if a = b then 
      
