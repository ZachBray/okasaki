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

  let member_old x s =
    let rec member_old = function
      | _, E -> false
      | x, T (l, y, r) ->
          if Elt.lt (x, y) then member_old (x, l)
          else if Elt.lt (y, x) then member_old (x, r)
          else true
    in
    member_old (x, s)

  let member x s =
    let rec member = function
      | _, E, None -> false
      | x, E, Some z -> Elt.eq (x, z)
      | x, T (l, y, r), z ->
          if Elt.lt (x, y) then member (x, l, z) else member (x, r, Some y)
    in
    member (x, s, None)

  let insert x s =
    let rec insert = function
      | x, E -> T (E, x, E)
      | x, (T (l, y, r) as s) ->
          if Elt.lt (x, y) then T (insert (x, l), y, r)
          else if Elt.lt (y, x) then T (l, y, insert (x, r))
          else s
    in
    insert (x, s)
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

let%bench_module "UnbalancedSet.member" =
  ( module struct
    let%bench_fun "with multiple comparisons per branch" =
      let module UIS = UnbalancedSet (IntOrdered) in
      let open UIS in
      let s =
        List.range 0 1000
        |> List.fold ~init:empty ~f:(fun acc x -> insert x acc)
      in
      fun () -> ignore (member_old 333 s)

    let%bench_fun "with single comparison per branch" =
      let module UIS = UnbalancedSet (IntOrdered) in
      let open UIS in
      let s =
        List.range 0 1000
        |> List.fold ~init:empty ~f:(fun acc x -> insert x acc)
      in
      fun () -> ignore (member 333 s)
  end )
