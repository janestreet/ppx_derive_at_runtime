open! Base
include Ppx_derive_at_runtime_lib_intf.Definitions

(* Defines an instance of [Type]. We deliberately define these types here rather than
   giving concrete definitions in [Definitions]. It makes functor constraints easier to
   satisfy. I believe this is because getting the module aliases "just right" to refer to
   concrete, functor-generated types from multiple places is difficult. *)
module Type (Spec : sig
    type ('a, 'b) leaf
    type (!_, !_) node
    type ('a, 'b) conversion
  end) :
  Type
  with type ('a, 'b) leaf := ('a, 'b) Spec.leaf
  with type ('a, 'b) node := ('a, 'b) Spec.node
  with type ('a, 'b) conversion := ('a, 'b) Spec.conversion = struct
  open Spec

  module Tree = struct
    type ('a, 'tree) t =
      | Leaf : ('a, 'leaf) leaf -> ('a, 'leaf) t
      | Node : ('a, 'left) t * ('a, 'right) t -> ('a, ('left, 'right) node) t
  end

  module Root = struct
    type ('a, 'tree) t =
      { tree : ('a, 'tree) Tree.t
      ; convert : ('a, 'tree) conversion
      }
  end

  type _ t = T : ('a, _) Root.t -> 'a t

  module Fold (Acc : T2) = struct
    type 'a leaf_callback =
      { on_leaf : 'leaf 'name. ('a, 'leaf) leaf -> ('a, 'leaf) Acc.t }
    [@@unboxed]

    type 'a node_callback =
      { on_node :
          'left 'right.
          ('a, 'left) Acc.t -> ('a, 'right) Acc.t -> ('a, ('left, 'right) node) Acc.t
      }
    [@@unboxed]

    let rec fold
      : type a tree.
        (a, tree) Tree.t
        -> leaf:a leaf_callback
        -> node:a node_callback
        -> (a, tree) Acc.t
      =
      fun tree ~leaf:{ on_leaf } ~node:{ on_node } ->
      match tree with
      | Leaf leaf -> on_leaf leaf
      | Node (left, right) ->
        on_node
          (fold left ~leaf:{ on_leaf } ~node:{ on_node })
          (fold right ~leaf:{ on_leaf } ~node:{ on_node })
    ;;
  end
end

(* Defines an instance of [Types], mostly using [Type] above. *)
module Types (Value : Value) = struct
  module Tuple = struct
    module Part = struct
      type ('whole, 'part) t =
        { index : int
        ; value : 'part Value.t
        ; access : 'whole -> 'part
        }
    end

    include Type (struct
        type ('a, 'b) leaf = ('a, 'b) Part.t
        type ('a, 'b) node = 'a * 'b
        type ('tuple, 'pairs) conversion = 'pairs -> 'tuple
      end)
  end

  module Record = struct
    module Label = struct
      type ('record, 'label) t =
        { name : string
        ; value : 'label Value.t
        ; attribute : ('record, 'label) Value.label_attribute option
        ; access : 'record -> 'label
        }
    end

    include Type (struct
        type ('a, 'b) leaf = ('a, 'b) Label.t
        type ('a, 'b) node = 'a * 'b
        type ('record, 'pairs) conversion = 'pairs -> 'record
      end)
  end

  module Variant = struct
    module Args = struct
      type _ t =
        | Empty : unit t
        | Tuple : 'a Tuple.t -> 'a t
        | Record : 'a Record.t -> 'a t
    end

    module Constructor = struct
      type ('variant, 'cons) t =
        { name : string
        ; args : 'cons Args.t
        ; attribute : ('variant, 'cons) Value.constructor_attribute option
        ; create : 'cons -> 'variant
        }
    end

    include Type (struct
        type ('a, 'b) leaf = ('a, 'b) Constructor.t
        type ('a, 'b) node = ('a, 'b) Either.t
        type ('variant, 'eithers) conversion = 'variant -> 'eithers
      end)
  end

  module Poly_variant = struct
    module Arg = struct
      type (_, _) t =
        | Empty :
            { name : string
            ; attribute : ('poly_variant, unit) Value.row_attribute option
            }
            -> ('poly_variant, unit) t
        | Value :
            { name : string
            ; attribute : ('poly_variant, 'row) Value.row_attribute option
            ; value : 'row Value.t
            }
            -> ('poly_variant, 'row) t
        | Inherited : 'a Value.t -> ('poly_variant, 'a) t
    end

    module Row = struct
      type ('poly_variant, 'row) t =
        { arg : ('poly_variant, 'row) Arg.t
        ; create : 'row -> 'poly_variant
        }
    end

    include Type (struct
        type ('a, 'b) leaf = ('a, 'b) Row.t
        type ('a, 'b) node = ('a, 'b) Either.t
        type ('poly_variant, 'eithers) conversion = 'poly_variant -> 'eithers
      end)
  end
end

(* Defines [Of_basic(Basic).Derive]. Defined separately to reduce indentation. *)
module Derive_of_basic (Basic : Basic) = struct
  module Value = struct
    type 'a t = 'a Basic.t
    type 'a attribute = 'a Basic.attribute
    type (_, 'a) constructor_attribute = 'a Basic.attribute
    type (_, 'a) label_attribute = 'a Basic.attribute
    type (_, 'a) row_attribute = 'a Basic.attribute
    type 'a override = 'a Basic.override
  end

  module Types = Types (Value)
  open Types

  let empty nothing_of_t =
    Basic.map_unmap Basic.nothing ~to_:Nothing.unreachable_code ~of_:nothing_of_t
  ;;

  let recursive _ lazy_t = Basic.recursive lazy_t
  let with_attribute = Basic.with_attribute
  let override = Basic.override

  let maybe_with_attribute value option =
    match option with
    | None -> value
    | Some attr -> with_attribute value attr
  ;;

  module Product_acc = struct
    type ('whole, 'tree) t =
      { value : 'tree Value.t
      ; access : 'whole -> 'tree
      }

    let both left right =
      { value = Basic.both left.value right.value
      ; access = (fun whole -> left.access whole, right.access whole)
      }
    ;;
  end

  module Fold_tuple = Tuple.Fold (Product_acc)
  module Fold_record = Record.Fold (Product_acc)

  let tuple (Tuple.T root) =
    let ({ value; access } : _ Product_acc.t) =
      Fold_tuple.fold
        root.tree
        ~leaf:{ on_leaf = (fun { value; access; index = _ } -> { value; access }) }
        ~node:{ on_node = Product_acc.both }
    in
    Basic.map_unmap value ~to_:root.convert ~of_:access
  ;;

  let record (Record.T root) =
    let ({ value; access } : _ Product_acc.t) =
      Fold_record.fold
        root.tree
        ~leaf:
          { on_leaf =
              (fun { value; access; attribute; name = _ } ->
                { value = maybe_with_attribute value attribute; access })
          }
        ~node:{ on_node = Product_acc.both }
    in
    Basic.map_unmap value ~to_:root.convert ~of_:access
  ;;

  module Sum_acc = struct
    type ('whole, 'tree) t =
      { value : 'tree Value.t
      ; create : 'tree -> 'whole
      }

    let either left right =
      { value = Basic.either left.value right.value
      ; create =
          (function
            | First x -> left.create x
            | Second y -> right.create y)
      }
    ;;
  end

  module Fold_variant = Variant.Fold (Sum_acc)
  module Fold_poly_variant = Poly_variant.Fold (Sum_acc)

  let variant (Variant.T root) =
    let ({ value; create } : _ Sum_acc.t) =
      Fold_variant.fold
        root.tree
        ~leaf:
          { on_leaf =
              (fun (type variant cons)
                ({ args; create; attribute; name = _ } :
                  (variant, cons) Variant.Constructor.t)
                : (variant, cons) Sum_acc.t ->
                let (value : cons Value.t) =
                  match args with
                  | Empty -> Basic.unit
                  | Tuple t -> tuple t
                  | Record r -> record r
                in
                { value = maybe_with_attribute value attribute; create })
          }
        ~node:{ on_node = Sum_acc.either }
    in
    Basic.map_unmap value ~to_:create ~of_:root.convert
  ;;

  let poly_variant (Poly_variant.T root) =
    let ({ value; create } : _ Sum_acc.t) =
      Fold_poly_variant.fold
        root.tree
        ~leaf:
          { on_leaf =
              (fun (type poly_variant row)
                ({ arg; create } : (poly_variant, row) Poly_variant.Row.t)
                : (poly_variant, row) Sum_acc.t ->
                match arg with
                | Empty _ -> { value = Basic.unit; create }
                | Value { value; attribute; name = _ } ->
                  { value = maybe_with_attribute value attribute; create }
                | Inherited value -> { value; create })
          }
        ~node:{ on_node = Sum_acc.either }
    in
    Basic.map_unmap value ~to_:create ~of_:root.convert
  ;;
end

module Of_basic (Basic : Basic) = struct
  module Derive = Derive_of_basic (Basic)
end

let reraise exn pos =
  let backtrace = Backtrace.Exn.most_recent () in
  Exn.raise_with_original_backtrace
    (Error.create_s
       (Sexp.List
          [ Atom "[ppx_derive_at_runtime]: runtime error"
          ; Source_code_position.sexp_of_t (Source_code_position.of_pos pos)
          ; Exn.sexp_of_t exn
          ])
     |> Error.to_exn)
    backtrace
;;
