(* Derives sample values of types. Used for testing this library. A more thoroughly
   documented implementation of the same interface is found in [serialization.ml]. *)

open! Base

type 'a t = 'a list

module Export = struct
  let sample_int = [ -1; 0; 1 ]
  let sample_list elts = ([] :: List.map elts ~f:List.return) @ [ elts ]
end

include Ppx_derive_at_runtime_lib.Of_basic (struct
    type nonrec 'a t = 'a t
    type _ attribute = Nothing.t
    type _ override = Nothing.t

    let map_unmap t ~to_:f ~of_:_ = List.map t ~f
    let unit = [ () ]
    let nothing = []

    let both a b =
      let a = Array.of_list a in
      let b = Array.of_list b in
      let m = Array.length a in
      let n = Array.length b in
      List.init (Int.max m n) ~f:(fun i -> a.(i % m), b.(i % n))
    ;;

    let either a b =
      List.concat [ List.map a ~f:Either.first; List.map b ~f:Either.second ]
    ;;

    let recursive _ = []
    let with_attribute _ nothing = Nothing.unreachable_code nothing
    let override nothing = Nothing.unreachable_code nothing
  end)
