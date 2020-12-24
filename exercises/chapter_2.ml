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

  (* This function returns a complete tree of depth [d]
     filled with [x]s. *)
  let rec complete = function
    | _, 0 -> E
    | x, d ->
        let sub_tree = complete (x, d - 1) in
        T (sub_tree, x, sub_tree)

  let rec create2 = function
    | x, 0 -> (E, T (E, x, E))
    | x, n ->
        let sub_tree, sub_tree' = create2 (x, (n - 1) / 2) in
        if n % 2 = 1 then (T (sub_tree, x, sub_tree), T (sub_tree', x, sub_tree))
        else (T (sub_tree', x, sub_tree), T (sub_tree', x, sub_tree'))

  let rec of_size (x, n) =
    let tree, _ = create2 (x, n) in
    tree

  let to_string_hum s = sexp_of_t s |> Sexp.to_string_hum
end

module Tests = struct
  module UIS = UnbalancedSet (IntOrdered)
  open UIS

  let flip f x y = f y x

  let set_of_primes_under_10 =
    [ 2; 3; 7; 5; 9 ] |> List.fold ~init:empty ~f:(flip insert)

  let%expect_test "Ex 2.2: member" =
    printf "let s = %s\n" (to_string_hum set_of_primes_under_10);
    for i = 0 to 10 do
      printf "member %d  s  = %b\n" i (member i set_of_primes_under_10)
    done;
    [%expect
      {|
    let s = (T E 2 (T E 3 (T (T E 5 E) 7 (T E 9 E))))
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

  let set_of_1000 =
    List.range 0 1000 |> List.fold ~init:empty ~f:(fun acc x -> insert x acc)

  let%bench "Ex 2.2: member - 2 cmp/node" = member 333 set_of_1000

  let%bench "Ex 2.2: member - 1 cmp/node" = member' 333 set_of_1000

  let%bench "Ex 2.3: insert existing - copy" = insert 333 set_of_1000

  let%bench "Ex 2.3: insert existing - exn" = insert' 333 set_of_1000

  let%bench "Ex 2.4: insert existing - exn + 1 cmp/node" =
    insert'' 333 set_of_1000

  let%expect_test "Ex 2.5(a): complete" =
    match complete (0, 3) with
    | E -> printf "fail"
    | T (l, _, r) as tree ->
        printf "%s\n" (to_string_hum tree);
        printf "phys_same l r = %b" (phys_same l r);
        [%expect {|
          (T (T (T E 0 E) 0 (T E 0 E)) 0 (T (T E 0 E) 0 (T E 0 E)))
          phys_same l r = true |}]

  let%expect_test "Ex 2.5(b): of_size" =
    printf "%s\n" (of_size (0, 0) |> to_string_hum);
    printf "%s\n" (of_size (1, 1) |> to_string_hum);
    printf "%s\n" (of_size (2, 2) |> to_string_hum);
    printf "%s\n" (of_size (3, 3) |> to_string_hum);
    printf "%s\n" (of_size (4, 4) |> to_string_hum);
    [%expect {|
      E
      (T E 1 E)
      (T (T E 2 E) 2 E)
      (T (T E 3 E) 3 (T E 3 E))
      (T (T (T E 4 E) 4 E) 4 (T E 4 E)) |}]
end
