open! Base
open Expect_test_helpers_base
open Base_quickcheck.Export
open Ppx_derive_at_runtime_example
open Comparison.Export
open Sample.Export
open Serialization.Export

let quickcheck_generator_int = Base_quickcheck.Generator.small_positive_or_zero_int

module type S0 = Test_expansion.S0

let test_sample (type a) (module T : S0 with type t = a) =
  List.iter T.sample ~f:(fun t -> print_s (T.serialization.sexp_of t))
;;

let test_serialization (type a) (module T : S0 with type t = a) =
  let module M = struct
    type t = T.t [@@deriving quickcheck]

    let equal = T.comparison.equal
    let sexp_of_t = T.serialization.sexp_of
  end
  in
  quickcheck_m (module M) ~f:(fun t ->
    let sexp = T.serialization.sexp_of t in
    let round_trip = T.serialization.of_sexp sexp in
    (* sexp_of and of_sexp round-trip *)
    require_equal (module M) t round_trip)
;;

let test_comparison (type a) ?(normalize = Fn.id) (module T : S0 with type t = a) =
  let module M = struct
    type t = T.t [@@deriving quickcheck]

    let sexp_of_t = T.serialization.sexp_of
  end
  in
  quickcheck_m (module M) ~f:(fun t ->
    (* equality is reflexive *)
    require (T.comparison.equal t t);
    (* compare is reflexive *)
    require_equal (module Int) (T.comparison.compare t t) 0;
    (* [normalize] is an equivalence class *)
    require (T.comparison.equal t (normalize t)));
  let module MM = struct
    type t = M.t * M.t [@@deriving quickcheck, sexp_of]
  end
  in
  let module Int_as_ordering = struct
    type t = int [@@deriving sexp_of]

    let equal a b = Comparable.lift Ordering.equal ~f:Ordering.of_int a b
  end
  in
  quickcheck_m (module MM) ~f:(fun (a, b) ->
    (* equality is symmetric *)
    require_equal (module Bool) (T.comparison.equal a b) (T.comparison.equal b a);
    (* comparison is antisymmetric *)
    require_equal
      (module Int_as_ordering)
      (T.comparison.compare a b)
      (-T.comparison.compare b a);
    (* equality and comparison do distinguish values *)
    if not (Poly.equal (normalize a) (normalize b))
    then (
      require (not (T.comparison.equal a b));
      require (T.comparison.compare a b <> 0)))
;;

let test0 ?normalize m : unit =
  test_sample m;
  test_serialization m;
  test_comparison ?normalize m
;;

module type S1 = Test_expansion.S1

let test1 (module M : S1) =
  test0
    (module struct
      type t = int M.t [@@deriving comparison, sample, serialization, quickcheck]
    end)
;;

module type S2 = Test_expansion.S2

let test2 (module M : S2) =
  test0
    (module struct
      type t = (int, int) M.t [@@deriving comparison, sample, serialization, quickcheck]
    end)
;;

module Type = Test_expansion.Type

let%expect_test _ =
  test2 (module Type);
  [%expect
    {|
    (A (() -1))
    (A ((-1) 0))
    (A ((0) 1))
    (A ((1) -1))
    (A ((-1 0 1) 0))
    (B ())
    (B ((-1 -1)))
    (B ((0 0)))
    (B ((1 1)))
    (B (
      (-1 -1)
      (0  0)
      (1  1)))
    (C (-1 -1 -1))
    (C (0 0 0))
    (C (1 1 1))
    |}]
;;

module Inherit = Test_expansion.Inherit

let%expect_test _ =
  test1 (module Inherit);
  [%expect
    {|
    (A (() -1))
    (A ((-1) 0))
    (A ((0) 1))
    (A ((1) -1))
    (A ((-1 0 1) 0))
    (B ())
    (B ((-1 -1)))
    (B ((0 0)))
    (B ((1 1)))
    (B (
      (-1 -1)
      (0  0)
      (1  1)))
    (C (-1 -1 -1))
    (C (0 0 0))
    (C (1 1 1))
    D
    |}]
;;

module Record = Test_expansion.Record

let%expect_test _ =
  test0 (module Record);
  [%expect
    {|
    ((x -1)
     (y -1)
     (z -1))
    ((x 0)
     (y 0)
     (z 0))
    ((x 1)
     (y 1)
     (z 1))
    |}]
;;

module Variant = Test_expansion.Variant

let%expect_test _ =
  test0 (module Variant);
  [%expect
    {|
    A
    (B -1 -1 -1)
    (B 0 0 0)
    (B 1 1 1)
    (C
      (x -1)
      (y -1)
      (z -1))
    (C
      (x 0)
      (y 0)
      (z 0))
    (C
      (x 1)
      (y 1)
      (z 1))
    |}]
;;

module Empty = Test_expansion.Empty

let%expect_test _ =
  require_does_raise (fun () -> test0 (module Empty));
  [%expect {| "cannot generate value of empty type" |}]
;;

module Recursive = Test_expansion.Recursive

let%expect_test _ =
  test0 (module Recursive);
  [%expect
    {|
    (Leaf -1)
    (Leaf 0)
    (Leaf 1)
    |}]
;;

module Custom = Test_expansion.Custom

let%expect_test _ =
  test0 (module Custom);
  [%expect
    {|
    (-1 a)
    (0 z)
    (1 A)
    (-1 Z)
    (0 0)
    (1 9)
    (-1 _)
    |}]
;;

module Extension = Test_expansion.Extension

let%expect_test _ =
  test0 (module Extension);
  [%expect
    {|
    0
    1
    |}]
;;

module Attribute = Test_expansion.Attribute

let%expect_test _ =
  test0 (module Attribute) ~normalize:(fun (t : Attribute.t) ->
    match t with
    | A | B _ -> t
    | C { x = _; y } ->
      (* C {x;y} annotates [x] as [Ignore] for purpose of comparisons *)
      C { x = 0; y });
  [%expect
    {|
    (t alpha)
    (t (B papa))
    (t (B (romeo queue)))
    (t (C (xray -1) (y (yankee -1))))
    (t (C (xray 0) (y (yankee 0))))
    (t (C (xray 1) (y (yankee 1))))
    |}]
;;
