open! Base

type 'a t =
  { sexp_of : 'a -> Sexp.t
  ; of_sexp : Sexp.t -> 'a
  }

let create_m (type a) (module M : Sexpable.S with type t = a) =
  { sexp_of = M.sexp_of_t; of_sexp = M.t_of_sexp }
;;

let map_unmap { sexp_of; of_sexp } ~to_ ~of_ =
  { sexp_of = Fn.compose sexp_of of_; of_sexp = Fn.compose to_ of_sexp }
;;

module Export = struct
  let serialization_int = create_m (module Int)

  let serialization_list { sexp_of; of_sexp } =
    { sexp_of = List.sexp_of_t sexp_of; of_sexp = List.t_of_sexp of_sexp }
  ;;
end

module Named = struct
  type t = Named of string
end

(* Because sexp serialization is an intensional property of a type -- syntactic details
   like label names matter -- we cannot simply use the [Of_basic] functor. Instead, we use
   the [Types] and [Fold] functors from [Ppx_derive_at_runtime_lib].

   When constructing a function using a [Fold] functor, we do the folding outside [fun]
   or [function]. This way we traverse the type only once. *)
module Derive = struct
  open Ppx_derive_at_runtime_lib

  (* We must define the derived value type and attribute types we are using. *)
  module Value = struct
    type nonrec 'a t = 'a t
    type _ attribute = Named.t
    type (_, _) label_attribute = Named.t
    type (_, _) row_attribute = Named.t
    type (_, _) constructor_attribute = Named.t
    type 'a override = (module Sexpable.S with type t = 'a)
  end

  (* The [Types] functors gives us the GADT binary trees for tuples, records, variants,
     and polymorphic variants. *)
  module Types = Types (Value)
  open Types

  (* We derive [sexp_of] and [of_sexp] separately, and combine them into a [t] later.
     This makes for easier to read code than trying to derive both at once. *)

  (* How to derive [sexp_of]: *)
  module Sexp_of = struct
    module Product_acc = struct
      (* For product types, our accumulator function conses onto a list of sexps. *)
      type ('whole, _) t = 'whole -> Sexp.t list -> Sexp.t list
    end

    module Fold_tuple = Tuple.Fold (Product_acc)

    (* Conses tuple elements onto a sexp. Used for tuple values, and for inline tuples in
       variant constructor arguments. *)
    let tuple_sexps =
      Fold_tuple.fold
        ~leaf:
          { on_leaf =
              (fun { index = _; value; access } x sexps ->
                value.sexp_of (access x) :: sexps)
          }
        ~node:{ on_node = (fun left right x sexps -> left x (right x sexps)) }
    ;;

    let tuple (Tuple.T root) =
      let sexps_of = tuple_sexps root.tree in
      fun x -> Sexp.List (sexps_of x [])
    ;;

    module Fold_record = Record.Fold (Product_acc)

    (* Conses record labels, wrapped with their labels, onto a sexp. Used for record
       values, and for inline records in variant constructor arguments. *)
    let record_sexps =
      Fold_record.fold
        ~leaf:
          { on_leaf =
              (fun { name; value; attribute; access } x sexps ->
                let name =
                  match attribute with
                  | Some (Named name) -> name
                  | None -> name
                in
                List [ Atom name; value.sexp_of (access x) ] :: sexps)
          }
        ~node:{ on_node = (fun left right x sexps -> left x (right x sexps)) }
    ;;

    let record (Record.T root) =
      let sexps_of = record_sexps root.tree in
      fun x -> Sexp.List (sexps_of x [])
    ;;

    module Sum_acc = struct
      (* For sum types, our accumulator function dispatches on [Either.t]s. *)
      type (_, 'eithers) t = 'eithers -> Sexp.t
    end

    module Fold_variant = Variant.Fold (Sum_acc)

    (* Dispatches on variant structure to construct a sexp. Uses [tuple_sexps] and
       [record_sexps] from above. [on_leaf] callbacks usually need type annotations to
       guarantee they are polymorphic in the ['part] type parameters. *)
    let variant (Variant.T root) =
      let sexp_of =
        Fold_variant.fold
          root.tree
          ~leaf:
            { on_leaf =
                (fun (type part)
                  ({ name; attribute; args; create = _ } :
                    (_, part) Variant.Constructor.t)
                  : (part -> Sexp.t) ->
                  let name =
                    match attribute with
                    | Some (Named name) -> name
                    | None -> name
                  in
                  match args with
                  | Empty -> fun () -> Atom name
                  | Tuple (T root) ->
                    fun x -> List (Atom name :: tuple_sexps root.tree x [])
                  | Record (T root) ->
                    fun x -> List (Atom name :: record_sexps root.tree x []))
            }
          ~node:
            { on_node =
                (fun left right x ->
                  match x with
                  | First a -> left a
                  | Second b -> right b)
            }
      in
      fun x -> sexp_of (root.convert x)
    ;;

    module Fold_poly_variant = Poly_variant.Fold (Sum_acc)

    (* Dispatches on polymorphic variant structure to construct a sexp. [on_leaf]
       callbacks usually need type annotations to guarantee they are polymorphic in the
       ['part] and ['name] type parameters. *)
    let poly_variant (Poly_variant.T root) =
      let sexp_of =
        Fold_poly_variant.fold
          root.tree
          ~leaf:
            { on_leaf =
                (fun (type part)
                  ({ arg; create = _ } : (_, part) Poly_variant.Row.t)
                  : (part -> Sexp.t) ->
                  match arg with
                  | Empty { name; attribute } ->
                    let name =
                      match attribute with
                      | Some (Named name) -> name
                      | None -> name
                    in
                    fun () -> Atom name
                  | Value { name; attribute; value } ->
                    fun x ->
                      let name =
                        match attribute with
                        | Some (Named name) -> name
                        | None -> name
                      in
                      List [ Atom name; value.sexp_of x ]
                  | Inherited value -> value.sexp_of)
            }
          ~node:
            { on_node =
                (fun left right x ->
                  match x with
                  | First a -> left a
                  | Second b -> right b)
            }
      in
      fun x -> sexp_of (root.convert x)
    ;;
  end

  (* How to derive [of_sexp]: *)
  module Of_sexp = struct
    module Product_acc = struct
      (* For product types, our accumulator function parses a prefix of a sexp list and
         returns the result plus remaining sexps. *)
      type (_, 'pairs) t = Sexp.t list -> 'pairs * Sexp.t list
    end

    module Fold_tuple = Tuple.Fold (Product_acc)
    module Fold_record = Record.Fold (Product_acc)

    (* Most of tuple and record parsing is the same, so we share some helpers. *)

    let on_product_node left right sexps =
      let l, sexps = left sexps in
      let r, sexps = right sexps in
      (l, r), sexps
    ;;

    let product_of_sexp of_sexps convert sexp =
      match (sexp : Sexp.t) with
      | Atom _ -> raise_s [%message "bad sexp"]
      | List sexps ->
        (match of_sexps sexps with
         | x, [] -> convert x
         | _, _ :: _ -> raise_s [%message "bad sexp"])
    ;;

    let tuple (Tuple.T root) =
      let of_sexps =
        Fold_tuple.fold
          root.tree
          ~leaf:
            { on_leaf =
                (fun { index = _; value; access = _ } sexps ->
                  match sexps with
                  | [] -> raise_s [%message "bad sexp"]
                  | sexp :: sexps -> value.of_sexp sexp, sexps)
            }
          ~node:{ on_node = on_product_node }
      in
      product_of_sexp of_sexps root.convert
    ;;

    let record (Record.T root) =
      let of_sexps =
        Fold_record.fold
          root.tree
          ~leaf:
            { on_leaf =
                (fun { name; attribute; value; access = _ } sexps ->
                  let name =
                    match attribute with
                    | Some (Named name) -> name
                    | None -> name
                  in
                  match sexps with
                  | List [ Atom atom; sexp ] :: sexps when String.equal atom name ->
                    value.of_sexp sexp, sexps
                  | _ -> raise_s [%message "bad sexp"])
            }
          ~node:{ on_node = on_product_node }
      in
      product_of_sexp of_sexps root.convert
    ;;

    module Sum_acc = struct
      (* For sum types, our accumulator function parses a sexp to a value, or [None] if
         inapplicable. *)
      type ('whole, _) t = Sexp.t -> 'whole option
    end

    module Fold_variant = Variant.Fold (Sum_acc)
    module Fold_poly_variant = Poly_variant.Fold (Sum_acc)

    (* Variant and polymorphic variant "leaf type" parsing is nontrivial, so we define
       them outside the folds. *)

    let on_variant_leaf
      (type a b)
      ({ name; attribute; args; create } : (a, b) Variant.Constructor.t)
      : Sexp.t -> a option
      =
      let name =
        match attribute with
        | Some (Named name) -> name
        | None -> name
      in
      match args with
      | Empty ->
        (function
          | Atom atom when String.equal atom name -> Some (create ())
          | _ -> None)
      | Tuple t ->
        let of_sexp = tuple t in
        (function
          | List (Atom atom :: sexps) when String.equal atom name ->
            Some (create (of_sexp (List sexps)))
          | _ -> None)
      | Record r ->
        let of_sexp = record r in
        (function
          | List (Atom atom :: sexps) when String.equal atom name ->
            Some (create (of_sexp (List sexps)))
          | _ -> None)
    ;;

    let on_poly_variant_leaf (type a b) ({ arg; create } : (a, b) Poly_variant.Row.t)
      : Sexp.t -> a option
      =
      match arg with
      | Empty { name; attribute } ->
        let name =
          match attribute with
          | Some (Named name) -> name
          | None -> name
        in
        (function
          | Atom atom when String.equal atom name -> Some (create ())
          | _ -> None)
      | Value { name; attribute; value } ->
        let name =
          match attribute with
          | Some (Named name) -> name
          | None -> name
        in
        (function
          | List [ Atom atom; sexp ] when String.equal atom name ->
            Some (create (value.of_sexp sexp))
          | _ -> None)
      | Inherited value ->
        fun sexp -> Option.try_with (fun () -> create (value.of_sexp sexp))
    ;;

    (* Other details of sum type parsing are shared, so we can define some helpers. *)

    let on_sum_node left right sexp = Option.first_some (left sexp) (right sexp)

    let sum_of_sexp of_sexp sexp =
      match of_sexp sexp with
      | Some x -> x
      | None -> raise_s [%message "bad sexp"]
    ;;

    let variant (Variant.T root) =
      Fold_variant.fold
        root.tree
        ~leaf:{ on_leaf = on_variant_leaf }
        ~node:{ on_node = on_sum_node }
      |> sum_of_sexp
    ;;

    let poly_variant (Poly_variant.T root) =
      Fold_poly_variant.fold
        root.tree
        ~leaf:{ on_leaf = on_poly_variant_leaf }
        ~node:{ on_node = on_sum_node }
      |> sum_of_sexp
    ;;
  end

  (* After defining [sexp_of] and [of_sexp], the rest is relatively straightforward. *)

  let empty nothing_of_t =
    { sexp_of = (fun t -> Nothing.unreachable_code (nothing_of_t t))
    ; of_sexp = (fun (_ : Sexp.t) -> raise_s [%message "bad sexp"])
    }
  ;;

  let tuple t = { sexp_of = Sexp_of.tuple t; of_sexp = Of_sexp.tuple t }
  let record t = { sexp_of = Sexp_of.record t; of_sexp = Of_sexp.record t }
  let variant t = { sexp_of = Sexp_of.variant t; of_sexp = Of_sexp.variant t }

  let poly_variant t =
    { sexp_of = Sexp_of.poly_variant t; of_sexp = Of_sexp.poly_variant t }
  ;;

  let recursive _ lazy_t =
    { sexp_of = (fun x -> (Lazy.force lazy_t).sexp_of x)
    ; of_sexp = (fun sexp -> (Lazy.force lazy_t).of_sexp sexp)
    }
  ;;

  let with_attribute t (Named.Named name) =
    { sexp_of = (fun x -> List [ Atom name; t.sexp_of x ])
    ; of_sexp =
        (function
          | List [ Atom atom; sexp ] when String.equal atom name -> t.of_sexp sexp
          | _ -> raise_s [%message "bad sexp"])
    }
  ;;

  let override = create_m
end
