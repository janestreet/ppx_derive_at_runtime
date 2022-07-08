open! Base

module type S0 = sig
  type t [@@deriving quickcheck, comparison, sample, serialization]
end

module type S1 = sig
  type 'a t [@@deriving quickcheck, comparison, sample, serialization]
end

module type S2 = sig
  type ('a, 'b) t [@@deriving quickcheck, comparison, sample, serialization]
end

module Type : S2
module Inherit : S1
module Record : S0
module Variant : S0
module Empty : S0
module Recursive : S0
module Custom : S0
module Extension : S0

module Attribute : sig
  type t =
    | A
    | B of [ `P | `Q ]
    | C of
        { x : int
        ; y : int
        }

  include S0 with type t := t
end
