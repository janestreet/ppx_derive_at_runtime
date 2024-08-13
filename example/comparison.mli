(** Defines how to derive [compare] and [equal] using [ppx_derive_at_runtime].

    This is an example of an _extensional_ derivation: only the contents of a type matter,
    not syntactic details like constructor names, or whether a product type is represented
    as a tuple or as a record.

    Internally, it is implemented with [Ppx_derive_at_runtime_lib.Of_basic]. *)

open! Base

type 'a t =
  { compare : 'a -> 'a -> int
  ; equal : 'a -> 'a -> bool
  }

val create_m : (module Base.Comparable.S with type t = 'a) -> 'a t
val unmap : 'a t -> f:('b -> 'a) -> 'b t

module Export : sig
  val comparison_int : int t
  val comparison_list : 'a t -> 'a list t
end

module Always_equal : sig
  type t = Always_equal
end

(** Derives [t]. Types can be annotated with [[@comparison.override Always_equal]] to
    create a [t] that treats all values as equal. *)
include
  Ppx_derive_at_runtime_lib.S_with_basic_attribute
  with type 'a t := 'a t
   and type _ attribute := Nothing.t
   and type _ override := Always_equal.t
