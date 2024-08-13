(** Defines how to derive a [sample] list using [ppx_derive_at_runtime].

    This is an example of an _extensional_ derivation: only the contents of a type matter,
    not syntactic details like constructor names, or whether a product type is represented
    as a tuple or as a record.

    Internally, it is implemented with [Ppx_derive_at_runtime_lib.Of_basic]. *)

open! Base

type 'a t = 'a list

module Export : sig
  val sample_int : int t
  val sample_list : 'a t -> 'a list t
end

(** Derives [t]. Attributes are not supported. *)
include
  Ppx_derive_at_runtime_lib.S_with_basic_attribute
  with type 'a t := 'a t
   and type _ attribute := Nothing.t
   and type _ override := Nothing.t
