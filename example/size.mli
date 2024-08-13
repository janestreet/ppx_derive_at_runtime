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

(** Derives [t], with [[@size f]] to modify the size with [f] and [@size.override n] to
    produce the constant size [n]. *)
include
  Ppx_derive_at_runtime_lib.S_with_basic_attribute
  with type 'a t := 'a t
   and type _ attribute := int -> int
   and type _ override := int
