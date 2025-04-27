(** Derives how to derive [sexp_of] and [of_sexp] using [ppx_derive_at_runtime].

    This is an example of an _intensional_ derivation: syntactic details of a type may
    matter, such as constructor names, or whether a product type is represented as a tuple
    or as a record. *)

open! Base

type 'a t =
  { sexp_of : 'a -> Sexp.t
  ; of_sexp : Sexp.t -> 'a
  }

val create_m : (module Sexpable.S with type t = 'a) -> 'a t
val map_unmap : 'a t -> to_:('a -> 'b) -> of_:('b -> 'a) -> 'b t

module Export : sig
  val serialization_int : int t
  val serialization_list : 'a t -> 'a list t
end

module Named : sig
  type t = Named of string
end

(** Derives [t]. Types, record labels, variant constructors, and polymorphic variant rows
    may be annotated with [[@serialization Named "my-name"]]. In the case of record
    labels, variant constructors, and tagged polymorphic variant rows, this replaces the
    existing name. For types and inherited polymorphic variant rows, this wraps the sexp
    for the value with the given name, e.g. [(some value)] becomes
    [(my-name (some value))]. *)
include
  Ppx_derive_at_runtime_lib.S_with_basic_attribute
  with type 'a t := 'a t
  with type _ attribute := Named.t
  with type 'a override := (module Sexpable.S with type t = 'a)
