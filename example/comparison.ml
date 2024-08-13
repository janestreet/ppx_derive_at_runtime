open! Base

type 'a t =
  { compare : 'a -> 'a -> int
  ; equal : 'a -> 'a -> bool
  }

let create_m (type a) (module M : Base.Comparable.S with type t = a) =
  { compare = M.compare; equal = M.equal }
;;

let unmap { equal; compare } ~f =
  { compare = (fun a b -> Comparable.lift compare ~f a b)
  ; equal = (fun a b -> Comparable.lift equal ~f a b)
  }
;;

module Export = struct
  let comparison_int = create_m (module Int)

  let comparison_list { compare; equal } =
    { compare = List.compare compare; equal = List.equal equal }
  ;;
end

module Always_equal = struct
  type t = Always_equal
end

(* Because [equal] and [compare] are extensional properties of a type -- they only care
   about the contents of values, not syntax such as field names -- we can derive [t] using
   the [Of_basic] functor. *)
include Ppx_derive_at_runtime_lib.Of_basic (struct
    type nonrec 'a t = 'a t
    type _ attribute = Nothing.t
    type _ override = Always_equal.t

    let unit = create_m (module Unit)
    let nothing = create_m (module Nothing)
    let map_unmap t ~to_:_ ~of_:f = unmap t ~f

    let both
      (type a b)
      { compare = compare_a; equal = equal_a }
      { compare = compare_b; equal = equal_b }
      =
      { compare = [%compare: a * b]; equal = [%equal: a * b] }
    ;;

    let either a b =
      { compare = Either.compare a.compare b.compare
      ; equal = Either.equal a.equal b.equal
      }
    ;;

    let with_attribute _ = Nothing.unreachable_code

    let override Always_equal.Always_equal =
      { compare = (fun _ _ -> 0); equal = (fun _ _ -> true) }
    ;;

    let recursive lazy_t =
      { compare = (fun x y -> (force lazy_t).compare x y)
      ; equal = (fun x y -> (force lazy_t).equal x y)
      }
    ;;
  end)
