(* Problem 9. Pack consecutive duplicates of list elements into sublists. (medium) *)


open Printf

(* Example:
 pack ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"d";"e";"e";"e";"e"];;
- : string list list =
[["a"; "a"; "a"; "a"]; ["b"]; ["c"; "c"]; ["a"; "a"]; ["d"; "d"];
 ["e"; "e"; "e"; "e"]]
*)

let rec pack = function
  | [] -> []
  | [a] -> [[a]]
  | a :: (b :: _ as t) ->
    if a = b then 
      match pack t with
        | [] -> [[a;b]]
        | head :: tail ->
          (a :: head) :: tail
    else
      let l = pack t in
        [a] :: l

let rec pprint_list = function
  | [] -> ()
  | [a] -> printf "%s];\n" a
  | a :: t -> printf "%s, " a; pprint_list t

let rec pprint_listlist = function
  | [] -> ()
  | [a] -> printf "[" ; pprint_list a ; ()
  | a :: t -> printf "[" ; pprint_list a ; pprint_listlist t

let () = 
  let l = pack ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"d";"e";"e";"e";"e"] in
    pprint_listlist l ;

