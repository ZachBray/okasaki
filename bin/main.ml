open Batteries

(* Exercise 2.1 *)
let rec suffixes = function
    | [] -> [[]]
    | (_::xs) as ys -> ys :: suffixes xs

(* Exercises 2.2 - 2.6 *)
module type Ordered =
sig
    type t

    val eq: t * t -> bool
    val lt: t * t -> bool
    val leq: t * t -> bool
end

module IntOrdered: Ordered with type t = int =
struct
    type t = int

    let eq (x, y) = x = y
    let lt (x, y) = x < y
    let leq (x, y) = x <= y
end

module UnbalancedSet (Elt: Ordered)  =
struct
    type elt = Elt.t
    type t = E | T of t * elt * t

    let empty = E

    let rec member = function
        | _, E -> false
        | x, T (l, y, r) ->
            if Elt.lt (x, y) then member (x, l)
            else if Elt.lt (y, x) then member (x, r)
            else true

    (* Exercise 2.2 *)
    let member' (x, s) =
        let rec member' = function
            | _, E, None -> false
            | x, E, Some z -> Elt.eq (x, z)
            | x, T (l, y, r), z ->
                if Elt.lt (x, y) then member' (x, l, z)
                else member' (x, r, Some y)
        in member' (x, s, None)

    let rec insert = function
        | x, E -> T (E, x, E)
        | x, (T (l, y, r) as s) ->
            if Elt.lt (x, y) then T (insert (x, l), y, r)
            else if Elt.lt (y, x) then T (l, y, insert (x, r))
            else s
end

module UIS = UnbalancedSet(IntOrdered)

let () = print_endline (dump (suffixes [1;2;3;4]))
