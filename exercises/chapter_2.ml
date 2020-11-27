open Core_kernel

let rec suffixes = function [] -> [ [] ] | _ :: xs as ys -> ys :: suffixes xs

let%expect_test "Exercise 2.1" =
  print_endline
    (Sexp.to_string_hum
       (List.sexp_of_t (List.sexp_of_t Int.sexp_of_t) (suffixes [ 1; 2; 3; 4 ])));
  [%expect {| ((1 2 3 4) (2 3 4) (3 4) (4) ()) |}]

module type Ordered = sig
  type t [@@deriving sexp]

  val eq : t * t -> bool

  val lt : t * t -> bool

  val leq : t * t -> bool
end

module IntOrdered : Ordered with type t = int = struct
  type t = int [@@deriving sexp]

  let eq (x, y) = x = y

  let lt (x, y) = x < y

  let leq (x, y) = x <= y
end

module UnbalancedSet (Elt : Ordered) = struct
  type elt = Elt.t [@@deriving sexp]

  type t = E | T of t * elt * t [@@deriving sexp]

  let empty = E

  (* Original [member] function with up to 2 comparisions per node *)
  let member x s =
    let rec member_aux = function
      | _, E -> false
      | x, T (l, y, r) ->
          if Elt.lt (x, y) then member_aux (x, l)
          else if Elt.lt (y, x) then member_aux (x, r)
          else true
    in
    member_aux (x, s)

  (* Modified [member] function with 1 comparision per node *)
  let member' x s =
    let rec member_aux = function
      | _, E, None -> false
      | x, E, Some z -> Elt.eq (x, z)
      | x, T (l, y, r), z ->
          if Elt.lt (x, y) then member_aux (x, l, z)
          else member_aux (x, r, Some y)
    in
    member_aux (x, s, None)

  (* Original [insert] function with up to 2 comparisions per node
     and copying of structure when element already exists *)
  let insert x s =
    let rec insert_aux = function
      | x, E -> T (E, x, E)
      | x, (T (l, y, r) as s) ->
          if Elt.lt (x, y) then T (insert_aux (x, l), y, r)
          else if Elt.lt (y, x) then T (l, y, insert_aux (x, r))
          else s
    in
    insert_aux (x, s)

  exception AlreadyExists

  (* Modified [insert] function that avoids copying the tree
     structure when an element already exists by using an exception. *)
  let insert' x s =
    try
      let rec insert_aux x = function
        | E -> T (E, x, E)
        | T (l, y, r) ->
            if Elt.lt (x, y) then T (insert_aux x l, y, r)
            else if Elt.lt (y, x) then T (l, y, insert_aux x r)
            else raise AlreadyExists
      in
      insert_aux x s
    with AlreadyExists -> s

  (* Modified [insert] function that avoids copying, as above, and
     reduces the maximum number of comparisons (using the technique
     from [member'] *)
  let insert'' x s =
    try
      let rec insert_aux = function
        | x, E, None -> T (E, x, E)
        | x, E, Some z ->
            if Elt.eq (x, z) then raise AlreadyExists else T (E, x, E)
        | x, T (l, y, r), z ->
            if Elt.lt (x, y) then insert_aux (x, l, z)
            else insert_aux (x, r, Some y)
      in
      insert_aux (x, s, None)
    with AlreadyExists -> s
end

let%expect_test "Exercise 2.2" =
  let module UIS = UnbalancedSet (IntOrdered) in
  let open UIS in
  let s = T (T (E, 2, E), 3, T (E, 5, T (T (E, 7, E), 9, E))) in
  printf "let s = %s\n" (Sexp.to_string (sexp_of_t s));
  for i = 0 to 10 do
    printf "member %d  s  = %b\n" i (member i s)
  done;
  [%expect
    {|
    let s = (T(T E 2 E)3(T E 5(T(T E 7 E)9 E)))
    member 0  s  = false
    member 1  s  = false
    member 2  s  = true
    member 3  s  = true
    member 4  s  = false
    member 5  s  = true
    member 6  s  = false
    member 7  s  = true
    member 8  s  = false
    member 9  s  = true
    member 10  s  = false |}]

module Benchmarks = struct
  module UIS = UnbalancedSet (IntOrdered)
  open UIS

  let s =
    List.range 0 1000 |> List.fold ~init:empty ~f:(fun acc x -> insert x acc)

  let%bench "Ex 2.2: member - 2 cmp/node" = member 333 s

  let%bench "Ex 2.2: member - 1 cmp/node" = member' 333 s

  let%bench "Ex 2.3: insert existing - copy" = insert 333 s

  let%bench "Ex 2.3: insert existing - exn" = insert' 333 s

  let%bench "Ex 2.4: insert existing - exn + 1 cmp/node" = insert'' 333 s
end
