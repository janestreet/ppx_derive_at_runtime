open! Base
open Base_quickcheck.Export
open Ppx_derive_at_runtime_example
open Comparison.Export
open Sample.Export
open Serialization.Export

let quickcheck_generator_int = Base_quickcheck.Generator.small_positive_or_zero_int

module type S0 = sig
  type t [@@deriving quickcheck] [@@deriving_inline comparison, sample, serialization]

  include sig
    [@@@ocaml.warning "-32"]

    val comparison : t Ppx_derive_at_runtime_example.Comparison.t
    val sample : t Ppx_derive_at_runtime_example.Sample.t
    val serialization : t Ppx_derive_at_runtime_example.Serialization.t
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end

module type S1 = sig
  type 'a t [@@deriving quickcheck] [@@deriving_inline comparison, sample, serialization]

  include sig
    [@@@ocaml.warning "-32"]

    val comparison
      : 'a.
      'a Ppx_derive_at_runtime_example.Comparison.t
      -> 'a t Ppx_derive_at_runtime_example.Comparison.t

    val sample
      : 'a.
      'a Ppx_derive_at_runtime_example.Sample.t
      -> 'a t Ppx_derive_at_runtime_example.Sample.t

    val serialization
      : 'a.
      'a Ppx_derive_at_runtime_example.Serialization.t
      -> 'a t Ppx_derive_at_runtime_example.Serialization.t
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end

module type S2 = sig
  type ('a, 'b) t
  [@@deriving quickcheck] [@@deriving_inline comparison, sample, serialization]

  include sig
    [@@@ocaml.warning "-32"]

    val comparison
      : 'a 'b.
      'a Ppx_derive_at_runtime_example.Comparison.t
      -> 'b Ppx_derive_at_runtime_example.Comparison.t
      -> ('a, 'b) t Ppx_derive_at_runtime_example.Comparison.t

    val sample
      : 'a 'b.
      'a Ppx_derive_at_runtime_example.Sample.t
      -> 'b Ppx_derive_at_runtime_example.Sample.t
      -> ('a, 'b) t Ppx_derive_at_runtime_example.Sample.t

    val serialization
      : 'a 'b.
      'a Ppx_derive_at_runtime_example.Serialization.t
      -> 'b Ppx_derive_at_runtime_example.Serialization.t
      -> ('a, 'b) t Ppx_derive_at_runtime_example.Serialization.t
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end

module Type = struct
  type ('a, 'b) t =
    [ `A of 'a list * int
    | `B of ('b * int) list
    | `C of int * 'a * 'b
    ]
  [@@deriving quickcheck] [@@deriving_inline comparison, sample, serialization]

  let _ = fun (_ : ('a, 'b) t) -> ()

  module _ : Ppx_derive_at_runtime_lib.S = Ppx_derive_at_runtime_example.Comparison

  let comparison __'a_comparison__ __'b_comparison__ =
    try
      Ppx_derive_at_runtime_example.Comparison.Derive.poly_variant
        (T
           { tree =
               Node
                 ( Leaf
                     { arg =
                         Value
                           { name = "A"
                           ; attribute = None
                           ; value =
                               Ppx_derive_at_runtime_example.Comparison.Derive.tuple
                                 (T
                                    { tree =
                                        Node
                                          ( Leaf
                                              { index = 0
                                              ; value = comparison_list __'a_comparison__
                                              ; access = (fun (x, _) -> x)
                                              }
                                          , Leaf
                                              { index = 1
                                              ; value = comparison_int
                                              ; access = (fun (_, x) -> x)
                                              } )
                                    ; convert = (fun (x0, x1) -> x0, x1)
                                    })
                           }
                     ; create = (fun x -> `A x)
                     }
                 , Node
                     ( Leaf
                         { arg =
                             Value
                               { name = "B"
                               ; attribute = None
                               ; value =
                                   comparison_list
                                     (Ppx_derive_at_runtime_example.Comparison.Derive
                                      .tuple
                                        (T
                                           { tree =
                                               Node
                                                 ( Leaf
                                                     { index = 0
                                                     ; value = __'b_comparison__
                                                     ; access = (fun (x, _) -> x)
                                                     }
                                                 , Leaf
                                                     { index = 1
                                                     ; value = comparison_int
                                                     ; access = (fun (_, x) -> x)
                                                     } )
                                           ; convert = (fun (x0, x1) -> x0, x1)
                                           }))
                               }
                         ; create = (fun x -> `B x)
                         }
                     , Leaf
                         { arg =
                             Value
                               { name = "C"
                               ; attribute = None
                               ; value =
                                   Ppx_derive_at_runtime_example.Comparison.Derive.tuple
                                     (T
                                        { tree =
                                            Node
                                              ( Leaf
                                                  { index = 0
                                                  ; value = comparison_int
                                                  ; access = (fun (x, _, _) -> x)
                                                  }
                                              , Node
                                                  ( Leaf
                                                      { index = 1
                                                      ; value = __'a_comparison__
                                                      ; access = (fun (_, x, _) -> x)
                                                      }
                                                  , Leaf
                                                      { index = 2
                                                      ; value = __'b_comparison__
                                                      ; access = (fun (_, _, x) -> x)
                                                      } ) )
                                        ; convert = (fun (x0, (x1, x2)) -> x0, x1, x2)
                                        })
                               }
                         ; create = (fun x -> `C x)
                         } ) )
           ; convert =
               (function
                 | `A x -> First x
                 | `B x -> Second (First x)
                 | `C x -> Second (Second x))
           })
    with
    | exn -> Ppx_derive_at_runtime_lib.reraise exn Stdlib.__POS__
  ;;

  let _ = comparison

  module _ : Ppx_derive_at_runtime_lib.S = Ppx_derive_at_runtime_example.Sample

  let sample __'a_sample__ __'b_sample__ =
    try
      Ppx_derive_at_runtime_example.Sample.Derive.poly_variant
        (T
           { tree =
               Node
                 ( Leaf
                     { arg =
                         Value
                           { name = "A"
                           ; attribute = None
                           ; value =
                               Ppx_derive_at_runtime_example.Sample.Derive.tuple
                                 (T
                                    { tree =
                                        Node
                                          ( Leaf
                                              { index = 0
                                              ; value = sample_list __'a_sample__
                                              ; access = (fun (x, _) -> x)
                                              }
                                          , Leaf
                                              { index = 1
                                              ; value = sample_int
                                              ; access = (fun (_, x) -> x)
                                              } )
                                    ; convert = (fun (x0, x1) -> x0, x1)
                                    })
                           }
                     ; create = (fun x -> `A x)
                     }
                 , Node
                     ( Leaf
                         { arg =
                             Value
                               { name = "B"
                               ; attribute = None
                               ; value =
                                   sample_list
                                     (Ppx_derive_at_runtime_example.Sample.Derive.tuple
                                        (T
                                           { tree =
                                               Node
                                                 ( Leaf
                                                     { index = 0
                                                     ; value = __'b_sample__
                                                     ; access = (fun (x, _) -> x)
                                                     }
                                                 , Leaf
                                                     { index = 1
                                                     ; value = sample_int
                                                     ; access = (fun (_, x) -> x)
                                                     } )
                                           ; convert = (fun (x0, x1) -> x0, x1)
                                           }))
                               }
                         ; create = (fun x -> `B x)
                         }
                     , Leaf
                         { arg =
                             Value
                               { name = "C"
                               ; attribute = None
                               ; value =
                                   Ppx_derive_at_runtime_example.Sample.Derive.tuple
                                     (T
                                        { tree =
                                            Node
                                              ( Leaf
                                                  { index = 0
                                                  ; value = sample_int
                                                  ; access = (fun (x, _, _) -> x)
                                                  }
                                              , Node
                                                  ( Leaf
                                                      { index = 1
                                                      ; value = __'a_sample__
                                                      ; access = (fun (_, x, _) -> x)
                                                      }
                                                  , Leaf
                                                      { index = 2
                                                      ; value = __'b_sample__
                                                      ; access = (fun (_, _, x) -> x)
                                                      } ) )
                                        ; convert = (fun (x0, (x1, x2)) -> x0, x1, x2)
                                        })
                               }
                         ; create = (fun x -> `C x)
                         } ) )
           ; convert =
               (function
                 | `A x -> First x
                 | `B x -> Second (First x)
                 | `C x -> Second (Second x))
           })
    with
    | exn -> Ppx_derive_at_runtime_lib.reraise exn Stdlib.__POS__
  ;;

  let _ = sample

  module _ : Ppx_derive_at_runtime_lib.S = Ppx_derive_at_runtime_example.Serialization

  let serialization __'a_serialization__ __'b_serialization__ =
    try
      Ppx_derive_at_runtime_example.Serialization.Derive.poly_variant
        (T
           { tree =
               Node
                 ( Leaf
                     { arg =
                         Value
                           { name = "A"
                           ; attribute = None
                           ; value =
                               Ppx_derive_at_runtime_example.Serialization.Derive.tuple
                                 (T
                                    { tree =
                                        Node
                                          ( Leaf
                                              { index = 0
                                              ; value =
                                                  serialization_list __'a_serialization__
                                              ; access = (fun (x, _) -> x)
                                              }
                                          , Leaf
                                              { index = 1
                                              ; value = serialization_int
                                              ; access = (fun (_, x) -> x)
                                              } )
                                    ; convert = (fun (x0, x1) -> x0, x1)
                                    })
                           }
                     ; create = (fun x -> `A x)
                     }
                 , Node
                     ( Leaf
                         { arg =
                             Value
                               { name = "B"
                               ; attribute = None
                               ; value =
                                   serialization_list
                                     (Ppx_derive_at_runtime_example.Serialization.Derive
                                      .tuple
                                        (T
                                           { tree =
                                               Node
                                                 ( Leaf
                                                     { index = 0
                                                     ; value = __'b_serialization__
                                                     ; access = (fun (x, _) -> x)
                                                     }
                                                 , Leaf
                                                     { index = 1
                                                     ; value = serialization_int
                                                     ; access = (fun (_, x) -> x)
                                                     } )
                                           ; convert = (fun (x0, x1) -> x0, x1)
                                           }))
                               }
                         ; create = (fun x -> `B x)
                         }
                     , Leaf
                         { arg =
                             Value
                               { name = "C"
                               ; attribute = None
                               ; value =
                                   Ppx_derive_at_runtime_example.Serialization.Derive
                                   .tuple
                                     (T
                                        { tree =
                                            Node
                                              ( Leaf
                                                  { index = 0
                                                  ; value = serialization_int
                                                  ; access = (fun (x, _, _) -> x)
                                                  }
                                              , Node
                                                  ( Leaf
                                                      { index = 1
                                                      ; value = __'a_serialization__
                                                      ; access = (fun (_, x, _) -> x)
                                                      }
                                                  , Leaf
                                                      { index = 2
                                                      ; value = __'b_serialization__
                                                      ; access = (fun (_, _, x) -> x)
                                                      } ) )
                                        ; convert = (fun (x0, (x1, x2)) -> x0, x1, x2)
                                        })
                               }
                         ; create = (fun x -> `C x)
                         } ) )
           ; convert =
               (function
                 | `A x -> First x
                 | `B x -> Second (First x)
                 | `C x -> Second (Second x))
           })
    with
    | exn -> Ppx_derive_at_runtime_lib.reraise exn Stdlib.__POS__
  ;;

  let _ = serialization

  [@@@end]
end

module Inherit = struct
  type 'a t =
    [ (int, 'a) Type.t
    | `D
    ]
  [@@deriving quickcheck] [@@deriving_inline comparison, sample, serialization]

  let _ = fun (_ : 'a t) -> ()

  module _ : Ppx_derive_at_runtime_lib.S = Ppx_derive_at_runtime_example.Comparison

  let comparison __'a_comparison__ =
    try
      Ppx_derive_at_runtime_example.Comparison.Derive.poly_variant
        (T
           { tree =
               Node
                 ( Leaf
                     { arg = Inherited (Type.comparison comparison_int __'a_comparison__)
                     ; create =
                         (fun x -> (x : (int, 'a) Type.t :> [ (int, 'a) Type.t | `D ]))
                     }
                 , Leaf
                     { arg = Empty { name = "D"; attribute = None }
                     ; create = (fun () -> `D)
                     } )
           ; convert =
               (function
                 | #Type.t as x -> First x
                 | `D -> Second ())
           })
    with
    | exn -> Ppx_derive_at_runtime_lib.reraise exn Stdlib.__POS__
  ;;

  let _ = comparison

  module _ : Ppx_derive_at_runtime_lib.S = Ppx_derive_at_runtime_example.Sample

  let sample __'a_sample__ =
    try
      Ppx_derive_at_runtime_example.Sample.Derive.poly_variant
        (T
           { tree =
               Node
                 ( Leaf
                     { arg = Inherited (Type.sample sample_int __'a_sample__)
                     ; create =
                         (fun x -> (x : (int, 'a) Type.t :> [ (int, 'a) Type.t | `D ]))
                     }
                 , Leaf
                     { arg = Empty { name = "D"; attribute = None }
                     ; create = (fun () -> `D)
                     } )
           ; convert =
               (function
                 | #Type.t as x -> First x
                 | `D -> Second ())
           })
    with
    | exn -> Ppx_derive_at_runtime_lib.reraise exn Stdlib.__POS__
  ;;

  let _ = sample

  module _ : Ppx_derive_at_runtime_lib.S = Ppx_derive_at_runtime_example.Serialization

  let serialization __'a_serialization__ =
    try
      Ppx_derive_at_runtime_example.Serialization.Derive.poly_variant
        (T
           { tree =
               Node
                 ( Leaf
                     { arg =
                         Inherited
                           (Type.serialization serialization_int __'a_serialization__)
                     ; create =
                         (fun x -> (x : (int, 'a) Type.t :> [ (int, 'a) Type.t | `D ]))
                     }
                 , Leaf
                     { arg = Empty { name = "D"; attribute = None }
                     ; create = (fun () -> `D)
                     } )
           ; convert =
               (function
                 | #Type.t as x -> First x
                 | `D -> Second ())
           })
    with
    | exn -> Ppx_derive_at_runtime_lib.reraise exn Stdlib.__POS__
  ;;

  let _ = serialization

  [@@@end]
end

module Record = struct
  type t =
    { x : int
    ; y : int
    ; z : int
    }
  [@@deriving quickcheck] [@@deriving_inline comparison, sample, serialization]

  let _ = fun (_ : t) -> ()

  module _ : Ppx_derive_at_runtime_lib.S = Ppx_derive_at_runtime_example.Comparison

  let comparison =
    try
      Ppx_derive_at_runtime_example.Comparison.Derive.record
        (T
           { tree =
               Node
                 ( Leaf
                     { name = "x"
                     ; attribute = None
                     ; value = comparison_int
                     ; access = (fun x -> x.x)
                     }
                 , Node
                     ( Leaf
                         { name = "y"
                         ; attribute = None
                         ; value = comparison_int
                         ; access = (fun x -> x.y)
                         }
                     , Leaf
                         { name = "z"
                         ; attribute = None
                         ; value = comparison_int
                         ; access = (fun x -> x.z)
                         } ) )
           ; convert = (fun (x, (y, z)) -> { x; y; z })
           })
    with
    | exn -> Ppx_derive_at_runtime_lib.reraise exn Stdlib.__POS__
  ;;

  let _ = comparison

  module _ : Ppx_derive_at_runtime_lib.S = Ppx_derive_at_runtime_example.Sample

  let sample =
    try
      Ppx_derive_at_runtime_example.Sample.Derive.record
        (T
           { tree =
               Node
                 ( Leaf
                     { name = "x"
                     ; attribute = None
                     ; value = sample_int
                     ; access = (fun x -> x.x)
                     }
                 , Node
                     ( Leaf
                         { name = "y"
                         ; attribute = None
                         ; value = sample_int
                         ; access = (fun x -> x.y)
                         }
                     , Leaf
                         { name = "z"
                         ; attribute = None
                         ; value = sample_int
                         ; access = (fun x -> x.z)
                         } ) )
           ; convert = (fun (x, (y, z)) -> { x; y; z })
           })
    with
    | exn -> Ppx_derive_at_runtime_lib.reraise exn Stdlib.__POS__
  ;;

  let _ = sample

  module _ : Ppx_derive_at_runtime_lib.S = Ppx_derive_at_runtime_example.Serialization

  let serialization =
    try
      Ppx_derive_at_runtime_example.Serialization.Derive.record
        (T
           { tree =
               Node
                 ( Leaf
                     { name = "x"
                     ; attribute = None
                     ; value = serialization_int
                     ; access = (fun x -> x.x)
                     }
                 , Node
                     ( Leaf
                         { name = "y"
                         ; attribute = None
                         ; value = serialization_int
                         ; access = (fun x -> x.y)
                         }
                     , Leaf
                         { name = "z"
                         ; attribute = None
                         ; value = serialization_int
                         ; access = (fun x -> x.z)
                         } ) )
           ; convert = (fun (x, (y, z)) -> { x; y; z })
           })
    with
    | exn -> Ppx_derive_at_runtime_lib.reraise exn Stdlib.__POS__
  ;;

  let _ = serialization

  [@@@end]
end

module Variant = struct
  type t =
    | A
    | B of int * int * int
    | C of
        { x : int
        ; y : int
        ; z : int
        }
  [@@deriving quickcheck] [@@deriving_inline comparison, sample, serialization]

  let _ = fun (_ : t) -> ()

  module _ : Ppx_derive_at_runtime_lib.S = Ppx_derive_at_runtime_example.Comparison

  let comparison =
    try
      Ppx_derive_at_runtime_example.Comparison.Derive.variant
        (T
           { tree =
               Node
                 ( Leaf
                     { name = "A"
                     ; attribute = None
                     ; args = Empty
                     ; create = (fun () -> A)
                     }
                 , Node
                     ( Leaf
                         { name = "B"
                         ; attribute = None
                         ; args =
                             Tuple
                               (T
                                  { tree =
                                      Node
                                        ( Leaf
                                            { index = 0
                                            ; value = comparison_int
                                            ; access = (fun (x, _, _) -> x)
                                            }
                                        , Node
                                            ( Leaf
                                                { index = 1
                                                ; value = comparison_int
                                                ; access = (fun (_, x, _) -> x)
                                                }
                                            , Leaf
                                                { index = 2
                                                ; value = comparison_int
                                                ; access = (fun (_, _, x) -> x)
                                                } ) )
                                  ; convert = (fun (x0, (x1, x2)) -> x0, x1, x2)
                                  })
                         ; create = (fun (x0, x1, x2) -> B (x0, x1, x2))
                         }
                     , Leaf
                         { name = "C"
                         ; attribute = None
                         ; args =
                             Record
                               (T
                                  { tree =
                                      Node
                                        ( Leaf
                                            { name = "x"
                                            ; attribute = None
                                            ; value = comparison_int
                                            ; access = (fun (x, _, _) -> x)
                                            }
                                        , Node
                                            ( Leaf
                                                { name = "y"
                                                ; attribute = None
                                                ; value = comparison_int
                                                ; access = (fun (_, x, _) -> x)
                                                }
                                            , Leaf
                                                { name = "z"
                                                ; attribute = None
                                                ; value = comparison_int
                                                ; access = (fun (_, _, x) -> x)
                                                } ) )
                                  ; convert = (fun (x, (y, z)) -> x, y, z)
                                  })
                         ; create = (fun (x, y, z) -> C { x; y; z })
                         } ) )
           ; convert =
               (function
                 | A -> First ()
                 | B (__value_0__, __value_1__, __value_2__) ->
                   Second (First (__value_0__, __value_1__, __value_2__))
                 | C { x = __field_x__; y = __field_y__; z = __field_z__ } ->
                   Second (Second (__field_x__, __field_y__, __field_z__)))
           })
    with
    | exn -> Ppx_derive_at_runtime_lib.reraise exn Stdlib.__POS__
  ;;

  let _ = comparison

  module _ : Ppx_derive_at_runtime_lib.S = Ppx_derive_at_runtime_example.Sample

  let sample =
    try
      Ppx_derive_at_runtime_example.Sample.Derive.variant
        (T
           { tree =
               Node
                 ( Leaf
                     { name = "A"
                     ; attribute = None
                     ; args = Empty
                     ; create = (fun () -> A)
                     }
                 , Node
                     ( Leaf
                         { name = "B"
                         ; attribute = None
                         ; args =
                             Tuple
                               (T
                                  { tree =
                                      Node
                                        ( Leaf
                                            { index = 0
                                            ; value = sample_int
                                            ; access = (fun (x, _, _) -> x)
                                            }
                                        , Node
                                            ( Leaf
                                                { index = 1
                                                ; value = sample_int
                                                ; access = (fun (_, x, _) -> x)
                                                }
                                            , Leaf
                                                { index = 2
                                                ; value = sample_int
                                                ; access = (fun (_, _, x) -> x)
                                                } ) )
                                  ; convert = (fun (x0, (x1, x2)) -> x0, x1, x2)
                                  })
                         ; create = (fun (x0, x1, x2) -> B (x0, x1, x2))
                         }
                     , Leaf
                         { name = "C"
                         ; attribute = None
                         ; args =
                             Record
                               (T
                                  { tree =
                                      Node
                                        ( Leaf
                                            { name = "x"
                                            ; attribute = None
                                            ; value = sample_int
                                            ; access = (fun (x, _, _) -> x)
                                            }
                                        , Node
                                            ( Leaf
                                                { name = "y"
                                                ; attribute = None
                                                ; value = sample_int
                                                ; access = (fun (_, x, _) -> x)
                                                }
                                            , Leaf
                                                { name = "z"
                                                ; attribute = None
                                                ; value = sample_int
                                                ; access = (fun (_, _, x) -> x)
                                                } ) )
                                  ; convert = (fun (x, (y, z)) -> x, y, z)
                                  })
                         ; create = (fun (x, y, z) -> C { x; y; z })
                         } ) )
           ; convert =
               (function
                 | A -> First ()
                 | B (__value_0__, __value_1__, __value_2__) ->
                   Second (First (__value_0__, __value_1__, __value_2__))
                 | C { x = __field_x__; y = __field_y__; z = __field_z__ } ->
                   Second (Second (__field_x__, __field_y__, __field_z__)))
           })
    with
    | exn -> Ppx_derive_at_runtime_lib.reraise exn Stdlib.__POS__
  ;;

  let _ = sample

  module _ : Ppx_derive_at_runtime_lib.S = Ppx_derive_at_runtime_example.Serialization

  let serialization =
    try
      Ppx_derive_at_runtime_example.Serialization.Derive.variant
        (T
           { tree =
               Node
                 ( Leaf
                     { name = "A"
                     ; attribute = None
                     ; args = Empty
                     ; create = (fun () -> A)
                     }
                 , Node
                     ( Leaf
                         { name = "B"
                         ; attribute = None
                         ; args =
                             Tuple
                               (T
                                  { tree =
                                      Node
                                        ( Leaf
                                            { index = 0
                                            ; value = serialization_int
                                            ; access = (fun (x, _, _) -> x)
                                            }
                                        , Node
                                            ( Leaf
                                                { index = 1
                                                ; value = serialization_int
                                                ; access = (fun (_, x, _) -> x)
                                                }
                                            , Leaf
                                                { index = 2
                                                ; value = serialization_int
                                                ; access = (fun (_, _, x) -> x)
                                                } ) )
                                  ; convert = (fun (x0, (x1, x2)) -> x0, x1, x2)
                                  })
                         ; create = (fun (x0, x1, x2) -> B (x0, x1, x2))
                         }
                     , Leaf
                         { name = "C"
                         ; attribute = None
                         ; args =
                             Record
                               (T
                                  { tree =
                                      Node
                                        ( Leaf
                                            { name = "x"
                                            ; attribute = None
                                            ; value = serialization_int
                                            ; access = (fun (x, _, _) -> x)
                                            }
                                        , Node
                                            ( Leaf
                                                { name = "y"
                                                ; attribute = None
                                                ; value = serialization_int
                                                ; access = (fun (_, x, _) -> x)
                                                }
                                            , Leaf
                                                { name = "z"
                                                ; attribute = None
                                                ; value = serialization_int
                                                ; access = (fun (_, _, x) -> x)
                                                } ) )
                                  ; convert = (fun (x, (y, z)) -> x, y, z)
                                  })
                         ; create = (fun (x, y, z) -> C { x; y; z })
                         } ) )
           ; convert =
               (function
                 | A -> First ()
                 | B (__value_0__, __value_1__, __value_2__) ->
                   Second (First (__value_0__, __value_1__, __value_2__))
                 | C { x = __field_x__; y = __field_y__; z = __field_z__ } ->
                   Second (Second (__field_x__, __field_y__, __field_z__)))
           })
    with
    | exn -> Ppx_derive_at_runtime_lib.reraise exn Stdlib.__POS__
  ;;

  let _ = serialization

  [@@@end]
end

module Empty = struct
  type t = | [@@deriving_inline comparison, sample, serialization]

  let _ = fun (_ : t) -> ()

  module _ : Ppx_derive_at_runtime_lib.S = Ppx_derive_at_runtime_example.Comparison

  let comparison =
    try
      Ppx_derive_at_runtime_example.Comparison.Derive.empty (function (_ : t) -> .)
    with
    | exn -> Ppx_derive_at_runtime_lib.reraise exn Stdlib.__POS__
  ;;

  let _ = comparison

  module _ : Ppx_derive_at_runtime_lib.S = Ppx_derive_at_runtime_example.Sample

  let sample =
    try Ppx_derive_at_runtime_example.Sample.Derive.empty (function (_ : t) -> .) with
    | exn -> Ppx_derive_at_runtime_lib.reraise exn Stdlib.__POS__
  ;;

  let _ = sample

  module _ : Ppx_derive_at_runtime_lib.S = Ppx_derive_at_runtime_example.Serialization

  let serialization =
    try
      Ppx_derive_at_runtime_example.Serialization.Derive.empty (function (_ : t) -> .)
    with
    | exn -> Ppx_derive_at_runtime_lib.reraise exn Stdlib.__POS__
  ;;

  let _ = serialization

  [@@@end]

  let quickcheck_generator =
    Base_quickcheck.Generator.map Base_quickcheck.Generator.unit ~f:(fun _ ->
      raise_s [%message "cannot generate value of empty type"])
  ;;

  let quickcheck_observer = Base_quickcheck.Observer.opaque
  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end

module Recursive = struct
  type 'a node = 'a list

  and t =
    | Leaf of int
    | Node of t node
  [@@deriving quickcheck] [@@deriving_inline comparison, sample, serialization]

  let _ = fun (_ : 'a node) -> ()
  let _ = fun (_ : t) -> ()

  module _ : Ppx_derive_at_runtime_lib.S = Ppx_derive_at_runtime_example.Comparison

  let comparison_node, comparison =
    let rec __lazy_comparison_node__ __'a_comparison__ =
      lazy
        (try comparison_list __'a_comparison__ with
         | exn -> Ppx_derive_at_runtime_lib.reraise exn Stdlib.__POS__)
    and __lazy_comparison__ =
      lazy
        (try
           Ppx_derive_at_runtime_example.Comparison.Derive.variant
             (T
                { tree =
                    Node
                      ( Leaf
                          { name = "Leaf"
                          ; attribute = None
                          ; args =
                              Tuple
                                (T
                                   { tree =
                                       Leaf
                                         { index = 0
                                         ; value = comparison_int
                                         ; access = (fun x -> x)
                                         }
                                   ; convert = (fun x0 -> x0)
                                   })
                          ; create = (fun x0 -> Leaf x0)
                          }
                      , Leaf
                          { name = "Node"
                          ; attribute = None
                          ; args =
                              Tuple
                                (T
                                   { tree =
                                       Leaf
                                         { index = 0
                                         ; value =
                                             Ppx_derive_at_runtime_example.Comparison
                                             .Derive
                                             .recursive
                                               "node"
                                               (__lazy_comparison_node__
                                                  (Ppx_derive_at_runtime_example
                                                   .Comparison
                                                   .Derive
                                                   .recursive
                                                     "t"
                                                     __lazy_comparison__))
                                         ; access = (fun x -> x)
                                         }
                                   ; convert = (fun x0 -> x0)
                                   })
                          ; create = (fun x0 -> Node x0)
                          } )
                ; convert =
                    (function
                      | Leaf __value_0__ -> First __value_0__
                      | Node __value_0__ -> Second __value_0__)
                })
         with
         | exn -> Ppx_derive_at_runtime_lib.reraise exn Stdlib.__POS__)
    in
    ( (fun a -> Stdlib.Lazy.force (__lazy_comparison_node__ a))
    , Stdlib.Lazy.force __lazy_comparison__ )
  ;;

  let _ = comparison_node
  and _ = comparison

  module _ : Ppx_derive_at_runtime_lib.S = Ppx_derive_at_runtime_example.Sample

  let sample_node, sample =
    let rec __lazy_sample_node__ __'a_sample__ =
      lazy
        (try sample_list __'a_sample__ with
         | exn -> Ppx_derive_at_runtime_lib.reraise exn Stdlib.__POS__)
    and __lazy_sample__ =
      lazy
        (try
           Ppx_derive_at_runtime_example.Sample.Derive.variant
             (T
                { tree =
                    Node
                      ( Leaf
                          { name = "Leaf"
                          ; attribute = None
                          ; args =
                              Tuple
                                (T
                                   { tree =
                                       Leaf
                                         { index = 0
                                         ; value = sample_int
                                         ; access = (fun x -> x)
                                         }
                                   ; convert = (fun x0 -> x0)
                                   })
                          ; create = (fun x0 -> Leaf x0)
                          }
                      , Leaf
                          { name = "Node"
                          ; attribute = None
                          ; args =
                              Tuple
                                (T
                                   { tree =
                                       Leaf
                                         { index = 0
                                         ; value =
                                             Ppx_derive_at_runtime_example.Sample.Derive
                                             .recursive
                                               "node"
                                               (__lazy_sample_node__
                                                  (Ppx_derive_at_runtime_example.Sample
                                                   .Derive
                                                   .recursive
                                                     "t"
                                                     __lazy_sample__))
                                         ; access = (fun x -> x)
                                         }
                                   ; convert = (fun x0 -> x0)
                                   })
                          ; create = (fun x0 -> Node x0)
                          } )
                ; convert =
                    (function
                      | Leaf __value_0__ -> First __value_0__
                      | Node __value_0__ -> Second __value_0__)
                })
         with
         | exn -> Ppx_derive_at_runtime_lib.reraise exn Stdlib.__POS__)
    in
    ( (fun a -> Stdlib.Lazy.force (__lazy_sample_node__ a))
    , Stdlib.Lazy.force __lazy_sample__ )
  ;;

  let _ = sample_node
  and _ = sample

  module _ : Ppx_derive_at_runtime_lib.S = Ppx_derive_at_runtime_example.Serialization

  let serialization_node, serialization =
    let rec __lazy_serialization_node__ __'a_serialization__ =
      lazy
        (try serialization_list __'a_serialization__ with
         | exn -> Ppx_derive_at_runtime_lib.reraise exn Stdlib.__POS__)
    and __lazy_serialization__ =
      lazy
        (try
           Ppx_derive_at_runtime_example.Serialization.Derive.variant
             (T
                { tree =
                    Node
                      ( Leaf
                          { name = "Leaf"
                          ; attribute = None
                          ; args =
                              Tuple
                                (T
                                   { tree =
                                       Leaf
                                         { index = 0
                                         ; value = serialization_int
                                         ; access = (fun x -> x)
                                         }
                                   ; convert = (fun x0 -> x0)
                                   })
                          ; create = (fun x0 -> Leaf x0)
                          }
                      , Leaf
                          { name = "Node"
                          ; attribute = None
                          ; args =
                              Tuple
                                (T
                                   { tree =
                                       Leaf
                                         { index = 0
                                         ; value =
                                             Ppx_derive_at_runtime_example.Serialization
                                             .Derive
                                             .recursive
                                               "node"
                                               (__lazy_serialization_node__
                                                  (Ppx_derive_at_runtime_example
                                                   .Serialization
                                                   .Derive
                                                   .recursive
                                                     "t"
                                                     __lazy_serialization__))
                                         ; access = (fun x -> x)
                                         }
                                   ; convert = (fun x0 -> x0)
                                   })
                          ; create = (fun x0 -> Node x0)
                          } )
                ; convert =
                    (function
                      | Leaf __value_0__ -> First __value_0__
                      | Node __value_0__ -> Second __value_0__)
                })
         with
         | exn -> Ppx_derive_at_runtime_lib.reraise exn Stdlib.__POS__)
    in
    ( (fun a -> Stdlib.Lazy.force (__lazy_serialization_node__ a))
    , Stdlib.Lazy.force __lazy_serialization__ )
  ;;

  let _ = serialization_node
  and _ = serialization

  [@@@end]
end

module Custom = struct
  type t =
    int
    * (char
      [@comparison.custom Comparison.create_m (module Char)]
      [@sample.custom String.to_list "azAZ09_"]
      [@serialization.custom Serialization.create_m (module Char)])
  [@@deriving quickcheck] [@@deriving_inline comparison, sample, serialization]

  let _ = fun (_ : t) -> ()

  module _ : Ppx_derive_at_runtime_lib.S = Ppx_derive_at_runtime_example.Comparison

  let comparison =
    try
      Ppx_derive_at_runtime_example.Comparison.Derive.tuple
        (T
           { tree =
               Node
                 ( Leaf { index = 0; value = comparison_int; access = (fun (x, _) -> x) }
                 , Leaf
                     { index = 1
                     ; value =
                         (Comparison.create_m (module Char)
                          : char Ppx_derive_at_runtime_example.Comparison.t)
                     ; access = (fun (_, x) -> x)
                     } )
           ; convert = (fun (x0, x1) -> x0, x1)
           })
    with
    | exn -> Ppx_derive_at_runtime_lib.reraise exn Stdlib.__POS__
  ;;

  let _ = comparison

  module _ : Ppx_derive_at_runtime_lib.S = Ppx_derive_at_runtime_example.Sample

  let sample =
    try
      Ppx_derive_at_runtime_example.Sample.Derive.tuple
        (T
           { tree =
               Node
                 ( Leaf { index = 0; value = sample_int; access = (fun (x, _) -> x) }
                 , Leaf
                     { index = 1
                     ; value =
                         (String.to_list "azAZ09_"
                          : char Ppx_derive_at_runtime_example.Sample.t)
                     ; access = (fun (_, x) -> x)
                     } )
           ; convert = (fun (x0, x1) -> x0, x1)
           })
    with
    | exn -> Ppx_derive_at_runtime_lib.reraise exn Stdlib.__POS__
  ;;

  let _ = sample

  module _ : Ppx_derive_at_runtime_lib.S = Ppx_derive_at_runtime_example.Serialization

  let serialization =
    try
      Ppx_derive_at_runtime_example.Serialization.Derive.tuple
        (T
           { tree =
               Node
                 ( Leaf
                     { index = 0; value = serialization_int; access = (fun (x, _) -> x) }
                 , Leaf
                     { index = 1
                     ; value =
                         (Serialization.create_m (module Char)
                          : char Ppx_derive_at_runtime_example.Serialization.t)
                     ; access = (fun (_, x) -> x)
                     } )
           ; convert = (fun (x0, x1) -> x0, x1)
           })
    with
    | exn -> Ppx_derive_at_runtime_lib.reraise exn Stdlib.__POS__
  ;;

  let _ = serialization

  [@@@end]
end

module Extension = struct
  type t =
    (char
    [@comparison.custom Comparison.unmap [%comparison: int] ~f:Char.to_int]
    [@serialization.custom
      Serialization.map_unmap [%serialization: int] ~to_:Char.of_int_exn ~of_:Char.to_int]
    [@sample.custom List.filter_map [%sample: int] ~f:Char.of_int])
  [@@deriving quickcheck] [@@deriving_inline comparison, sample, serialization]

  let _ = fun (_ : t) -> ()

  module _ : Ppx_derive_at_runtime_lib.S = Ppx_derive_at_runtime_example.Comparison

  let comparison =
    try
      (Comparison.unmap
         (comparison_int : int Ppx_derive_at_runtime_example.Comparison.t)
         ~f:Char.to_int
       : char Ppx_derive_at_runtime_example.Comparison.t)
    with
    | exn -> Ppx_derive_at_runtime_lib.reraise exn Stdlib.__POS__
  ;;

  let _ = comparison

  module _ : Ppx_derive_at_runtime_lib.S = Ppx_derive_at_runtime_example.Sample

  let sample =
    try
      (List.filter_map
         (sample_int : int Ppx_derive_at_runtime_example.Sample.t)
         ~f:Char.of_int
       : char Ppx_derive_at_runtime_example.Sample.t)
    with
    | exn -> Ppx_derive_at_runtime_lib.reraise exn Stdlib.__POS__
  ;;

  let _ = sample

  module _ : Ppx_derive_at_runtime_lib.S = Ppx_derive_at_runtime_example.Serialization

  let serialization =
    try
      (Serialization.map_unmap
         (serialization_int : int Ppx_derive_at_runtime_example.Serialization.t)
         ~to_:Char.of_int_exn
         ~of_:Char.to_int
       : char Ppx_derive_at_runtime_example.Serialization.t)
    with
    | exn -> Ppx_derive_at_runtime_lib.reraise exn Stdlib.__POS__
  ;;

  let _ = serialization

  [@@@end]
end

module Attribute = struct
  module Q = struct
    type t = [ `Q ]

    let sexp_of_t `Q = Sexp.Atom "queue"

    let t_of_sexp = function
      | Sexp.Atom "queue" -> `Q
      | _ -> assert false
    ;;
  end

  type r = [ `Q ]
  [@@serialization.override (module Q)]
  [@@deriving comparison, sample, serialization, quickcheck]

  type t =
    | A [@serialization Named "alpha"]
    | B of [ `P [@serialization Named "papa"] | (r[@serialization Named "romeo"]) ]
    | C of
        { x : (int[@comparison.override Always_equal]) [@serialization Named "xray"]
        ; y : (int[@serialization Named "yankee"])
        }
  [@@deriving quickcheck]
  [@@serialization Named "t"]
  [@@deriving_inline comparison, sample, serialization]

  let _ = fun (_ : t) -> ()

  module _ : Ppx_derive_at_runtime_lib.S = Ppx_derive_at_runtime_example.Comparison

  let comparison =
    try
      Ppx_derive_at_runtime_example.Comparison.Derive.variant
        (T
           { tree =
               Node
                 ( Leaf
                     { name = "A"
                     ; attribute = None
                     ; args = Empty
                     ; create = (fun () -> A)
                     }
                 , Node
                     ( Leaf
                         { name = "B"
                         ; attribute = None
                         ; args =
                             Tuple
                               (T
                                  { tree =
                                      Leaf
                                        { index = 0
                                        ; value =
                                            Ppx_derive_at_runtime_example.Comparison
                                            .Derive
                                            .poly_variant
                                              (T
                                                 { tree =
                                                     Node
                                                       ( Leaf
                                                           { arg =
                                                               Empty
                                                                 { name = "P"
                                                                 ; attribute = None
                                                                 }
                                                           ; create = (fun () -> `P)
                                                           }
                                                       , Leaf
                                                           { arg = Inherited comparison_r
                                                           ; create =
                                                               (fun x ->
                                                                 (x : r :> [ `P | r ]))
                                                           } )
                                                 ; convert =
                                                     (function
                                                       | `P -> First ()
                                                       | #r as x -> Second x)
                                                 })
                                        ; access = (fun x -> x)
                                        }
                                  ; convert = (fun x0 -> x0)
                                  })
                         ; create = (fun x0 -> B x0)
                         }
                     , Leaf
                         { name = "C"
                         ; attribute = None
                         ; args =
                             Record
                               (T
                                  { tree =
                                      Node
                                        ( Leaf
                                            { name = "x"
                                            ; attribute = None
                                            ; value =
                                                Ppx_derive_at_runtime_example.Comparison
                                                .Derive
                                                .override
                                                  Always_equal
                                            ; access = (fun (x, _) -> x)
                                            }
                                        , Leaf
                                            { name = "y"
                                            ; attribute = None
                                            ; value = comparison_int
                                            ; access = (fun (_, x) -> x)
                                            } )
                                  ; convert = (fun (x, y) -> x, y)
                                  })
                         ; create = (fun (x, y) -> C { x; y })
                         } ) )
           ; convert =
               (function
                 | A -> First ()
                 | B __value_0__ -> Second (First __value_0__)
                 | C { x = __field_x__; y = __field_y__ } ->
                   Second (Second (__field_x__, __field_y__)))
           })
    with
    | exn -> Ppx_derive_at_runtime_lib.reraise exn Stdlib.__POS__
  ;;

  let _ = comparison

  module _ : Ppx_derive_at_runtime_lib.S = Ppx_derive_at_runtime_example.Sample

  let sample =
    try
      Ppx_derive_at_runtime_example.Sample.Derive.variant
        (T
           { tree =
               Node
                 ( Leaf
                     { name = "A"
                     ; attribute = None
                     ; args = Empty
                     ; create = (fun () -> A)
                     }
                 , Node
                     ( Leaf
                         { name = "B"
                         ; attribute = None
                         ; args =
                             Tuple
                               (T
                                  { tree =
                                      Leaf
                                        { index = 0
                                        ; value =
                                            Ppx_derive_at_runtime_example.Sample.Derive
                                            .poly_variant
                                              (T
                                                 { tree =
                                                     Node
                                                       ( Leaf
                                                           { arg =
                                                               Empty
                                                                 { name = "P"
                                                                 ; attribute = None
                                                                 }
                                                           ; create = (fun () -> `P)
                                                           }
                                                       , Leaf
                                                           { arg = Inherited sample_r
                                                           ; create =
                                                               (fun x ->
                                                                 (x : r :> [ `P | r ]))
                                                           } )
                                                 ; convert =
                                                     (function
                                                       | `P -> First ()
                                                       | #r as x -> Second x)
                                                 })
                                        ; access = (fun x -> x)
                                        }
                                  ; convert = (fun x0 -> x0)
                                  })
                         ; create = (fun x0 -> B x0)
                         }
                     , Leaf
                         { name = "C"
                         ; attribute = None
                         ; args =
                             Record
                               (T
                                  { tree =
                                      Node
                                        ( Leaf
                                            { name = "x"
                                            ; attribute = None
                                            ; value = sample_int
                                            ; access = (fun (x, _) -> x)
                                            }
                                        , Leaf
                                            { name = "y"
                                            ; attribute = None
                                            ; value = sample_int
                                            ; access = (fun (_, x) -> x)
                                            } )
                                  ; convert = (fun (x, y) -> x, y)
                                  })
                         ; create = (fun (x, y) -> C { x; y })
                         } ) )
           ; convert =
               (function
                 | A -> First ()
                 | B __value_0__ -> Second (First __value_0__)
                 | C { x = __field_x__; y = __field_y__ } ->
                   Second (Second (__field_x__, __field_y__)))
           })
    with
    | exn -> Ppx_derive_at_runtime_lib.reraise exn Stdlib.__POS__
  ;;

  let _ = sample

  module _ : Ppx_derive_at_runtime_lib.S = Ppx_derive_at_runtime_example.Serialization

  let serialization =
    try
      Ppx_derive_at_runtime_example.Serialization.Derive.with_attribute
        (Ppx_derive_at_runtime_example.Serialization.Derive.variant
           (T
              { tree =
                  Node
                    ( Leaf
                        { name = "A"
                        ; attribute = Some (Named "alpha")
                        ; args = Empty
                        ; create = (fun () -> A)
                        }
                    , Node
                        ( Leaf
                            { name = "B"
                            ; attribute = None
                            ; args =
                                Tuple
                                  (T
                                     { tree =
                                         Leaf
                                           { index = 0
                                           ; value =
                                               Ppx_derive_at_runtime_example.Serialization
                                               .Derive
                                               .poly_variant
                                                 (T
                                                    { tree =
                                                        Node
                                                          ( Leaf
                                                              { arg =
                                                                  Empty
                                                                    { name = "P"
                                                                    ; attribute =
                                                                        Some
                                                                          (Named "papa")
                                                                    }
                                                              ; create = (fun () -> `P)
                                                              }
                                                          , Leaf
                                                              { arg =
                                                                  Inherited
                                                                    (Ppx_derive_at_runtime_example
                                                                     .Serialization
                                                                     .Derive
                                                                     .with_attribute
                                                                       serialization_r
                                                                       (Named "romeo"))
                                                              ; create =
                                                                  (fun x ->
                                                                    (x : r :> [ `P | r ]))
                                                              } )
                                                    ; convert =
                                                        (function
                                                          | `P -> First ()
                                                          | #r as x -> Second x)
                                                    })
                                           ; access = (fun x -> x)
                                           }
                                     ; convert = (fun x0 -> x0)
                                     })
                            ; create = (fun x0 -> B x0)
                            }
                        , Leaf
                            { name = "C"
                            ; attribute = None
                            ; args =
                                Record
                                  (T
                                     { tree =
                                         Node
                                           ( Leaf
                                               { name = "x"
                                               ; attribute = Some (Named "xray")
                                               ; value = serialization_int
                                               ; access = (fun (x, _) -> x)
                                               }
                                           , Leaf
                                               { name = "y"
                                               ; attribute = None
                                               ; value =
                                                   Ppx_derive_at_runtime_example
                                                   .Serialization
                                                   .Derive
                                                   .with_attribute
                                                     serialization_int
                                                     (Named "yankee")
                                               ; access = (fun (_, x) -> x)
                                               } )
                                     ; convert = (fun (x, y) -> x, y)
                                     })
                            ; create = (fun (x, y) -> C { x; y })
                            } ) )
              ; convert =
                  (function
                    | A -> First ()
                    | B __value_0__ -> Second (First __value_0__)
                    | C { x = __field_x__; y = __field_y__ } ->
                      Second (Second (__field_x__, __field_y__)))
              }))
        (Named "t")
    with
    | exn -> Ppx_derive_at_runtime_lib.reraise exn Stdlib.__POS__
  ;;

  let _ = serialization

  [@@@end]
end

module type Extension_in_signature = sig
  type 'a t

  [@@@ocaml.warning "-32"]

  [@@@expand_inline:
    val create : [%comparison: 'a t] * [%sample: 'a t] * [%serialization: 'a t] -> 'a t]

  val create
    :  'a t Ppx_derive_at_runtime_example.Comparison.t
       * 'a t Ppx_derive_at_runtime_example.Sample.t
       * 'a t Ppx_derive_at_runtime_example.Serialization.t
    -> 'a t

  [@@@end]
end
