open! Base

(** The type of [size] operations. *)
type 'a t = 'a -> int

(** Transforming a size operation is straightforward function composition. *)
let unmap t ~f x = t (f x)

(** Strings and lists have natural size operations. *)
module Export = struct
  let size_string = String.length
  let size_list size_elt list = List.sum (module Int) list ~f:size_elt
end

(** Defining the [Ignore] attribute type. *)
module Ignore = struct
  type t = Ignore
end

(** We provide straightforward combinators on [t] to define how to derive values. *)
include Ppx_derive_at_runtime_lib.Of_basic (struct
    type nonrec 'a t = 'a t
    type _ attribute = Ignore.t

    (** In general, we can derive for covariant, contravariant, or invariant types, so we
        may need to "map", "unmap", or both. In this case, we only need to "unmap". *)
    let map_unmap t ~to_:_ ~of_ = unmap t ~f:of_

    (** Unit has a trivial size. Using 1 instead of 0 -- or, for that matter, 2 or any
        other number -- is an arbitrary choice. *)
    let unit () = 1

    (** Nothing doesn't have a size. If that makes sense. *)
    let nothing = Nothing.unreachable_code

    (** Pairs add the sizes of both constituents. We arbitrarily choose not to add a
        constant for the pair constructor itself. *)
    let both at bt : (_ * _) t = fun (a, b) -> at a + bt b

    (** Variants just take the size of either part. Again, we arbitrarily choose not to
        increment for the variant constructor itself. *)
    let either at bt : (_, _) Either.t t = function
      | First a -> at a
      | Second b -> bt b
    ;;

    (** The [Ignore] attribute means we produce a constant size of zero. *)
    let with_attribute (type a) (_ : a t) (Ignore : Ignore.t) : a t = Fn.const 0

    (** For (mutually) recursive types, we force the lazy definition on demand. *)
    let recursive (type a) (at : a t Lazy.t) : a t = fun a -> (Lazy.force at) a
  end)
