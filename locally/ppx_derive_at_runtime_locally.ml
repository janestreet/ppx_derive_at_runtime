open! Base

let register_via_command_line here module_name =
  Ppx_derive_at_runtime.register_fully_qualified_runtime_module here module_name
  |> Ppxlib.Deriving.ignore
;;

(** Register the command-line flag. *)
let () =
  Ppxlib.Driver.add_arg
    "-derive-from-module"
    (String (register_via_command_line (Source_code_position.of_pos Stdlib.__POS__)))
    ~doc:"locally derive values using a runtime module"
;;
