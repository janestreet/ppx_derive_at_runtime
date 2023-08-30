(** Defines how to derive a [size] function using [ppx_derive_at_runtime]. *)
open! Base

(** The type of [size] operations. *)
type 'a t = 'a -> int

(** Transforms an ['a t] to a ['b t]. Because [t] is contravariant -- that is, ['a] is an
    input rather than an output -- we must "unmap" inputs of type ['b] to type ['a] to
    pass to the original ['a t]. *)
val unmap : 'a t -> f:('b -> 'a) -> 'b t

(** Size operations for builtin types. When deriving [size], [open] this module. *)
module Export : sig
  val size_string : string t
  val size_list : 'a t -> 'a list t
end

(** Used as an attribute when deriving size. Types annotated with [[@size Ignore]] are not
    included in the total. *)
module Ignore : sig
  type t = Ignore
end

(** Derives [t]. Per the definition of [S_with_basic_attribute], the [[@size]] attribute
    may be used the same way on types, record labels, variant constructors, and
    polymorphic variant rows. *)
include
  Ppx_derive_at_runtime_lib.S_with_basic_attribute
    with type 'a t := 'a t
     and type _ attribute := Ignore.t
