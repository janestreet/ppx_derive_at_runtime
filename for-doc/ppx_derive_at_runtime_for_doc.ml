open! Base

let () =
  Ppx_derive_at_runtime.register_fully_qualified_runtime_module
    [%here]
    "Ppx_derive_at_runtime_example.Size"
  |> Ppxlib.Deriving.ignore
;;
