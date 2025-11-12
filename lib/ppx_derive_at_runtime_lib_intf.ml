(** This library defines runtime support for [ppx_derive_at_runtime]. It provides types
    and values that generated code refers to. It also provides functors that help clients
    define new ppxes based on [ppx_derive_at_runtime]. *)

open! Base

module Lazy = struct
  type 'a t = 'a Lazy.t

  type%template 'a t = 'a Portable_lazy.t [@@mode portable]
end

(** Module types re-exported below. *)
module Definitions = struct
  (** A simple set of definitions that can be used to derive values for a new type. Values
      derived using this interface are based on the contents of a type, but not other
      details of its structure. For example, variant constructor names and record label
      names are not taken into account.

      See the [Of_basic] functor, below. *)
  module type Basic = sig
    type 'a t
    type 'a attribute
    type 'a override

    (** Transforms an ['a t] to a ['b t].

        In covariant types, only [to_] is needed. Examples include [type 'a t = 'a list]
        and [type 'a t = Sexp.t -> 'a].

        In contravariant types, only [of_] is needed. Examples include
        [type 'a t = 'a -> Sexp.t].

        Invariant types need both [to_] and [of_]. Examples include [type 'a t = 'a -> 'a]
        and [type 'a t = 'a ref].

        Monomorphic types need neither [to_] nor [of_]. Examples include
        [type 'a t = unit]. *)
    val map_unmap : 'a t -> to_:('a -> 'b) -> of_:('b -> 'a) -> 'b t

    (** Derives values from singleton types. *)
    val unit : unit t

    (** Derives values from uninhabited types. *)
    val nothing : Nothing.t t

    (** Derives values from product types. *)
    val both : 'a t -> 'b t -> ('a * 'b) t

    (** Derives values from sum types. *)
    val either : 'a t -> 'b t -> ('a, 'b) Either.t t

    (** Derives values from recursive types. *)
    val recursive : 'a t Lazy.t -> 'a t

    (** Derives values from types annotated with an appropriate attribute. See
        [../README.mdx]. *)
    val with_attribute : 'a t -> 'a attribute -> 'a t

    (** Derives values from override attributes. *)
    val override : 'a override -> 'a t
  end

  (** Types of values provided by each type to be derived. *)
  module type Value = sig
    (** The type being derived. *)
    type 'a t

    (** Attributes placed on a type ['a]. *)
    type 'a attribute

    (** Attributes placed on a label of type ['label] of a record of type ['record]. *)
    type ('record, 'label) label_attribute

    (** Attributes placed on a constructor of type ['cons] of a variant of type
        ['variant]. The type ['cons] depends on the arguments of the variant constructor;
        see [Types.Variant], below. *)
    type ('variant, 'cons) constructor_attribute

    (** Attributes placed on a row of type ['row] of a polymorphic variant of type
        ['poly_variant]. *)
    type ('poly_variant, 'row) row_attribute

    (** Attribute that overrides deriving for ['a]. *)
    type 'a override
  end

  (** Specifies a product or sum type using a GADT binary tree representation. We use a
      single representation with several parameters so that tuples, records, variants, and
      polymorphic variants may all be understood with one set of types. *)
  module type Type_without_fold = sig
    (** The data stored at the leaf of a type. In product types, this represents a single
        field. In variant types, this represents a single case.

        ['whole] is the type of the enclosing product or sum type.

        ['part] is the type of the data stored in the case or field. *)
    type ('whole, 'part) leaf

    (** A node combining two subtrees of derived values. For product types, this is
        ['left * 'right]. For sum types, this is [('left, 'right) Either.t].

        The [!] below means that [node] must be injective in both type parameters. This is
        required for the [Tree.t] GADT to typecheck, below. *)
    type (!'left, !'right) node

    (** Conversion function between a type and its generic "tree-shaped" type. In product
        types, this is a constructor mapping a ['tree] of tuples to the ['whole] type. In
        sum types, this is a conversion from a ['whole] value to a ['tree] of [Either.t]
        types. *)
    type ('whole, 'tree) conversion

    (** The recursive tree structure of a product or sum type representation. *)
    module Tree : sig
      type ('whole, 'tree) t =
        | Leaf : ('whole, 'part) leaf -> ('whole, 'part) t
        | Node :
            ('whole, 'left) t * ('whole, 'right) t
            -> ('whole, ('left, 'right) node) t
    end

    (** The root of a product or sum type binary tree representation. Contains the binary
        tree, and a conversion function between the "flat" type and the binary tree type
        using pairs or [Either]s. *)
    module Root : sig
      type ('whole, 'tree) t =
        { tree : ('whole, 'tree) Tree.t
        ; convert : ('whole, 'tree) conversion
        }
    end

    (** A product or sum type representation, hiding the internal tree type. *)
    type _ t = T : ('whole, 'tree) Root.t -> 'whole t
  end

  (** Signature of a fold over a binary tree from [Type_without_fold], above. *)
  module type%template Fold = sig
    (** Binary tree types. *)

    type (_, _) node
    type (_, _) leaf
    type (_, _) tree

    (** Accumulator type of the fold. *)
    type (_, _) acc

    (** Polymorphic callbacks used in recursion over the GADT. *)

    type 'whole leaf_callback =
      { on_leaf : 'part. ('whole, 'part) leaf @ p -> ('whole, 'part) acc @ p }
    [@@unboxed]

    type 'whole node_callback =
      { on_node :
          'left 'right.
          ('whole, 'left) acc @ p
          -> ('whole, 'right) acc @ p
          -> ('whole, ('left, 'right) node) acc @ p
      }
    [@@unboxed]

    (** Folds over a type representation. *)
    val fold
      :  ('whole, 'tree) tree @ p
      -> leaf:'whole leaf_callback @ p
      -> node:'whole node_callback @ p
      -> ('whole, 'tree) acc @ p
  end
  [@@modality p = (portable, nonportable)]

  (** Extends [Type_without_fold] with a functor providing [Fold]. *)
  module type Type = sig
    include Type_without_fold

    (** Produces a generic fold over [t], given a result type. *)
    module%template.portable [@modality p] Fold (Acc : T2) :
      Fold
      [@modality p]
      with type ('whole, 'tree) acc := ('whole, 'tree) Acc.t
       and type ('left, 'right) node := ('left, 'right) node
       and type ('whole, 'part) leaf := ('whole, 'part) leaf
       and type ('whole, 'tree) tree := ('whole, 'tree) Tree.t
  end

  module type Types = sig
    module Value : Value

    (** GADT representation of tuples as [int]-named product types. *)
    module Tuple : sig
      module Part : sig
        type ('whole, 'part) t =
          { index : int
          ; value : 'part Value.t
          ; access : 'whole -> 'part
          }
      end

      include
        Type
        with type ('left, 'right) node := 'left * 'right
        with type ('whole, 'part) leaf := ('whole, 'part) Part.t
        with type ('tuple, 'pairs) conversion := 'pairs -> 'tuple
    end

    (** GADT representation of records as [string]-named product types. *)
    module Record : sig
      module Label : sig
        type ('record, 'label) t =
          { name : string
          ; value : 'label Value.t
          ; attribute : ('record, 'label) Value.label_attribute option
          ; access : 'record -> 'label
          }
      end

      include
        Type
        with type ('left, 'right) node := 'left * 'right
        with type ('record, 'label) leaf := ('record, 'label) Label.t
        with type ('record, 'pairs) conversion := 'pairs -> 'record
    end

    (** GADT representation of variants as [string]-named sum types. *)
    module Variant : sig
      module Args : sig
        type _ t =
          | Empty : unit t
          | Tuple : 'a Tuple.t -> 'a t
          | Record : 'a Record.t -> 'a t
      end

      module Constructor : sig
        type ('variant, 'cons) t =
          { name : string
          ; args : 'cons Args.t
          ; attribute : ('variant, 'cons) Value.constructor_attribute option
          ; create : 'cons -> 'variant
          }
      end

      include
        Type
        with type ('left, 'right) node := ('left, 'right) Either.t
        with type ('variant, 'cons) leaf := ('variant, 'cons) Constructor.t
        with type ('variant, 'eithers) conversion := 'variant -> 'eithers
    end

    (** GADT representation of polymorphic variants as sum types. Tagged rows have
        [string] names and inherited rows have [unit] names. *)
    module Poly_variant : sig
      module Arg : sig
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

      module Row : sig
        type ('poly_variant, 'row) t =
          { arg : ('poly_variant, 'row) Arg.t
          ; create : 'row -> 'poly_variant
          }
      end

      include
        Type
        with type ('left, 'right) node := ('left, 'right) Either.t
        with type ('poly_variant, 'row) leaf := ('poly_variant, 'row) Row.t
        with type ('poly_variant, 'eithers) conversion := 'poly_variant -> 'eithers
    end
  end

  (** Module type of the [Derive] submodule that each module for a derived value must
      provide. *)
  module type%template Derive = sig
    module Value : Value
    module Types : Types with module Value := Value

    (** Derives values for empty types. *)
    val empty : ('a -> Nothing.t) @ p -> 'a Value.t @ p

    (** Derives values for tuple types. *)
    val tuple : 'a Types.Tuple.t @ p -> 'a Value.t @ p

    (** Derives values for record types. *)
    val record : 'a Types.Record.t @ p -> 'a Value.t @ p

    (** Derives values for variant types. *)
    val variant : 'a Types.Variant.t @ p -> 'a Value.t @ p

    (** Derives values for polymorphic variant types. *)
    val poly_variant : 'a Types.Poly_variant.t @ p -> 'a Value.t @ p

    (** Derives values for recursive types, including their name. *)
    val recursive : string -> ('a Value.t Lazy.t[@mode p]) -> 'a Value.t @ p

    (** Derives values for types annotated with [Value.attribute]. *)
    val with_attribute : 'a Value.t @ p -> 'a Value.attribute -> 'a Value.t @ p

    (** Derives values from override attributes. *)
    val override : 'a Value.override -> 'a Value.t @ p
  end
  [@@modality p = (portable, nonportable)]

  (** Module type that a runtime module must satisfy for [ppx_derive_at_runtime]. *)
  module type%template S = sig
    type 'a t

    module Derive : Derive [@modality p] with type 'a Value.t = 'a t
  end
  [@@mode p = (portable, nonportable)]

  (** Specialization of [S] for [Of_basic], where all attributes share a single type. *)
  module type S_with_basic_attribute = sig
    type 'a attribute
    type 'a override

    include
      S
      with type 'a Derive.Value.attribute = 'a attribute
       and type (_, 'a) Derive.Value.constructor_attribute = 'a attribute
       and type (_, 'a) Derive.Value.label_attribute = 'a attribute
       and type (_, 'a) Derive.Value.row_attribute = 'a attribute
       and type 'a Derive.Value.override = 'a override
  end
end

module type Ppx_derive_at_runtime_lib = sig @@ portable
  include module type of struct
    include Definitions
  end

  (** Simple interface for deriving values from types. Does not provide access to
      constructor names, record labels, or other syntactic details; see [Basic] above.
      Does not require the caller to interface with GADTs defined above. *)
  module Of_basic (Basic : Basic) :
    S_with_basic_attribute
    with type 'a t := 'a Basic.t
     and type 'a attribute := 'a Basic.attribute
     and type 'a override := 'a Basic.override

  (** Defines GADT types and fold helpers for given derived value and attribute types. *)
  module Types (Value : Value) : Types with module Value := Value

  (**/**)

  (** Used in generated code. Wraps an exception in an error message, reporting the
      location where [ppx_derive_at_runtime] generated code that raised. See
      [Stdlib.__POS__] for location format. *)
  val reraise : exn -> string * int * int * int -> _ @ portable
end
