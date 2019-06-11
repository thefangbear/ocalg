(* Problem 12. Decode a run-length encoded list. (medium)
  Given a run-length code list generated as specified in
  the previous problem, construct its uncompressed version.
*)

(* We can write the decode function in two ways
  1) the correct ^{TM} -way: use the Stream module in OCaml
  2) the lame way: use auxiliary functions that generate lists
*)

open Printf
open Stream

type 'a rle =
    | One of 'a
    | Many of int * 'a

let rec decode = fun l ->
  let const_stream k = Stream.from (fun _ -> Some k) in
    match l with
      | [] -> []
      | One a :: t -> [a] @ (decode t)
      | Many (cnt, a) :: t -> (Stream.npeek cnt a) @ (decode t)

