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

(* decode using system's Stream module *)
let rec decode = function
  | [] -> []
  | One a :: t -> [a] @ (decode t)
  | Many (cnt, a) :: t -> 
    (Stream.npeek cnt (
             Stream.from (fun _ -> Some a))) @ (decode t)

(* our vanilla implementation of a stream *)

module ConstStream =
  struct
    type 'a stream  = Cons of 'a * 'a stream 
    let create = fun n -> 
      let rec f = Cons (n, f) in f
    let head f = 
      let Cons (n, _) = f in n
    let tail f = 
      let Cons (_, t) = f in t
    let rec npeek f n = 
      match n with
        | 0 -> let Cons (a, _) = f in [a]
        | k -> let Cons (a, _) = f in
          a :: (npeek f (n - 1))
  end

let rec decode1 = function
  | [] -> []
  | One a :: t -> [a] @ (decode1 t)
  | Many (cnt, a) :: t ->
    (ConstStream.npeek (ConstStream.create a) cnt) @ (decode t)
