open! Base

(** [register_fully_qualified_runtime_module here id] creates a new [@@deriving] name
    based on [id], which is parsed as an OCaml module path. This can be used to create new
    ppxes based on [ppx_derive_at_runtime]. Errors at registration time are reported based
    on [here].

    See [../README.mdx] for further details. *)
val register_fully_qualified_runtime_module
  :  Source_code_position.t
  -> string
  -> Ppxlib.Deriving.t
