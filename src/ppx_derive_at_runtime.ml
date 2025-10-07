open! Base
open Ppxlib
open Ast_builder.Default

(** Copies source syntax to be used in generated code. Strips attributes and ensures all
    locations are marked as "ghost". **)
let copy =
  object
    inherit Ast_traverse.map
    method! location loc = { loc with loc_ghost = true }
    method! attributes _ = []
  end
;;

module Config = struct
  type t =
    { module_id : Longident.t (** runtime module name **)
    ; name : string (** name used in [@@deriving] and derived value names *)
    ; attribute_custom : (core_type, expression) Attribute.t
    (** [@foo.custom] attribute **)
    ; attribute_core_type : (core_type, expression) Attribute.t (** [@foo] on types **)
    ; attribute_clause : (constructor_declaration, expression) Attribute.t
    (** \@foo on variant clauses *)
    ; attribute_field : (label_declaration, expression) Attribute.t
    (** [@foo] on record fields *)
    ; attribute_row : (row_field, expression) Attribute.t
    (** [@foo] on polymorphic variant rows *)
    ; attribute_type_decl : (type_declaration, expression) Attribute.t
    (** [@@foo] on type declarations *)
    ; override_core_type : (core_type, expression) Attribute.t
    (** [@foo.override] on types **)
    ; override_type_decl : (type_declaration, expression) Attribute.t
    (** [@@foo.override] on type declarations *)
    }

  (** Parses a module path, with a useful error message. *)
  let parse_identifier string ~loc =
    match Ocaml_common.Parse.simple_module_path (Lexing.from_string string) with
    | id -> id
    | exception _ ->
      Location.raise_errorf
        ~loc
        "ppx_derive_at_runtime: runtime module must be a module path, got: %S"
        string
  ;;

  (** Extracts the [@@deriving] name from a module id: the part after all [.]s *)
  let name_of_module_id id ~loc =
    match (id : Longident.t) with
    | Lident name | Ldot (_, name) -> name
    | Lapply (_, _) ->
      Location.raise_errorf
        ~loc
        "ppx_derive_at_runtime: runtime module must not be a functor application, got: %s"
        (Longident.name id)
  ;;

  (** constructor *)
  let create ~here ~module_path =
    let (loc : location) = { loc_ghost = false; loc_start = here; loc_end = here } in
    let module_id = parse_identifier module_path ~loc in
    let name = String.uncapitalize (name_of_module_id module_id ~loc) in
    let expr =
      let open Ast_pattern in
      (* all relevant attributes expect a single expression as payload *)
      pstr (pstr_eval __ nil ^:: nil)
    in
    let attribute_custom = Attribute.declare (name ^ ".custom") Core_type expr Fn.id in
    let attribute_core_type = Attribute.declare name Core_type expr Fn.id in
    let attribute_clause = Attribute.declare name Constructor_declaration expr Fn.id in
    let attribute_field = Attribute.declare name Label_declaration expr Fn.id in
    let attribute_row = Attribute.declare name Rtag expr Fn.id in
    let attribute_type_decl = Attribute.declare name Type_declaration expr Fn.id in
    let override_core_type =
      Attribute.declare (name ^ ".override") Core_type expr Fn.id
    in
    let override_type_decl =
      Attribute.declare (name ^ ".override") Type_declaration expr Fn.id
    in
    { module_id
    ; name
    ; attribute_custom
    ; attribute_core_type
    ; attribute_clause
    ; attribute_field
    ; attribute_row
    ; attribute_type_decl
    ; override_core_type
    ; override_type_decl
    }
  ;;

  (** field accessors, except [name] *)

  let module_id t = t.module_id
  let attribute_core_type t = t.attribute_core_type
  let attribute_clause t = t.attribute_clause
  let attribute_custom t = t.attribute_custom
  let attribute_field t = t.attribute_field
  let attribute_row t = t.attribute_row
  let attribute_type_decl t = t.attribute_type_decl
  let override_core_type t = t.override_core_type
  let override_type_decl t = t.override_type_decl

  (** name used in [@@deriving] *)
  let name_of_deriving t = t.name

  (** name of derived value *)
  let name_of_value t name =
    (* special-case [t] to derive values with no suffix *)
    if String.equal name "t" then t.name else String.concat ~sep:"_" [ t.name; name ]
  ;;

  (** name of lazy value when deriving from mutually recursive types *)
  let name_of_lazy_value t name = Printf.sprintf "__lazy_%s__" (name_of_value t name)

  (** name of derived values for type variables in polymorphic types *)
  let name_of_type_variable t name = Printf.sprintf "__'%s_%s__" name t.name

  (** type of a derived value for some type *)
  let type_of_value t ~loc core_type =
    ptyp_constr ~loc { loc; txt = Ldot (t.module_id, "t") } [ copy#core_type core_type ]
  ;;

  (** reference to a value from the runtime module *)
  let runtime_value t ~loc name =
    pexp_ident ~loc { loc; txt = Ldot (Ldot (t.module_id, "Derive"), name) }
  ;;
end

(** Creates and traverses non-empty binary trees from lists. Used to turn tuples, record,
    variants, and polymorphic variants into GADT trees. *)
module Binary_tree = struct
  type 'a t =
    | Leaf of 'a
    | Node of 'a t * 'a t

  let rec create list =
    match list with
    | [] -> None
    | [ one ] -> Some (Leaf one)
    | _ :: _ :: _ ->
      let left, right = List.split_n list (List.length list / 2) in
      (* the below is always [Some] *)
      Option.map2 (create left) (create right) ~f:(fun l r -> Node (l, r))
  ;;

  let rec fold t ~one ~two =
    match t with
    | Leaf a -> one a
    | Node (l, r) -> two (fold l ~one ~two) (fold r ~one ~two)
  ;;
end

(** expands [@@deriving foo] in signatures *)
module Signature = struct
  let expand ~config ~loc:_ ~path:_ (_, tds) =
    let tds = List.map tds ~f:name_type_params_in_td in
    List.map tds ~f:(fun td ->
      let loc = td.ptype_loc in
      value_description
        ~loc
        ~name:(Loc.map ~f:(Config.name_of_value config) td.ptype_name)
        ~type_:
          (Ppx_helpers.combinator_type_of_type_declaration
             td
             ~f:(Config.type_of_value config)
           |> Ppx_helpers.Polytype.to_core_type)
        ~prim:[]
      |> psig_value ~loc)
  ;;

  (** Expand [%foo: _] extensions. *)
  let extension ~config =
    Extension.declare
      (Config.name_of_deriving config)
      Core_type
      Ast_pattern.(ptyp __)
      (fun ~loc ~path:_ core_type ->
        [%type: [%t Config.type_of_value config ~loc core_type]])
  ;;
end

(** expands [@@deriving foo] in structures *)
module Structure = struct
  (** Report errors that should not be able to occur. Generate an error extension node so
      that expansion may continue for, e.g., merlin-compatibility. *)
  let impossible ~loc desc =
    let message = Printf.sprintf "[ppx_derive_at_runtime] bug: %s" desc in
    [%expr [%ocaml.error [%e estring ~loc message]]]
  ;;

  (** Report errors due to missing support for some feature. Generate an error extension
      node so that expansion may continue for, e.g., merlin-compatibility. *)
  let unsupported ~loc ~config desc =
    let message =
      Printf.sprintf "deriving %s: %s not supported" (Config.name_of_deriving config) desc
    in
    [%expr [%ocaml.error [%e estring ~loc message]]]
  ;;

  (** A conservative approximation of which expressions can raise at runtime. *)
  let rec can_raise expr =
    match
      Ppxlib_jane.Shim.Expression_desc.of_parsetree expr.pexp_desc ~loc:expr.pexp_loc
    with
    (* constants and no-ops that cannot raise (except perhaps out of memory) *)
    | Pexp_unreachable | Pexp_ident _ | Pexp_constant _ | Pexp_function _ | Pexp_lazy _ ->
      false
    (* type annotations, recur on actual value *)
    | Pexp_newtype ((_ : string loc), (_ : Ppxlib_jane.jkind_annotation option), expr)
    | Pexp_constraint (expr, (_ : core_type option), _)
    | Pexp_coerce (expr, (_ : core_type option), (_ : core_type))
    | Pexp_stack expr -> can_raise expr
    (* conservatively assume that other constructs we haven't special-cased might raise.
    *)
    | _ -> true
  ;;

  (** If something raises while a type is "deriving at runtime", we want to report where
      this happened because it is otherwise hard to track down these errors, especially if
      they happen at module initialization time. *)
  let wrap_runtime_error ~loc expr =
    match can_raise expr with
    | false -> expr
    | true ->
      [%expr
        try [%e expr] with
        | exn -> Ppx_derive_at_runtime_lib.reraise exn Stdlib.__POS__]
  ;;

  (** record syntax constructor *)
  let erecord ~loc alist =
    let fields =
      List.map alist ~f:(fun (name, expr) -> { loc; txt = Lident name }, expr)
    in
    pexp_record ~loc fields None
  ;;

  (** option syntax constructor *)
  let eoption ~loc option =
    match option with
    | None -> [%expr None]
    | Some expr -> [%expr Some [%e expr]]
  ;;

  (** construct syntax for accessing slot [i] of a tuple of length [n] *)
  let expand_tuple_get ~loc ~i ~n =
    let pat =
      let body =
        List.init n ~f:(fun j -> if i = j then pvar ~loc "x" else ppat_any ~loc)
      in
      ppat_tuple_opt ~loc body |> Option.value ~default:(punit ~loc)
    in
    eabstract ~loc [ pat ] (evar ~loc "x")
  ;;

  (** Expand a reference to a derived value of another type. Handles recursive wrapping of
      mutually recursive derivers. *)
  let expand_reference id params ~loc ~config ~murec =
    match id.txt with
    | Lident name when Set.mem murec name ->
      [ estring ~loc name
      ; type_constr_conv ~loc id params ~f:(Config.name_of_lazy_value config)
      ]
      |> eapply ~loc (Config.runtime_value config ~loc "recursive")
    | _ -> type_constr_conv ~loc id params ~f:(Config.name_of_value config)
  ;;

  module Row = struct
    type t =
      | Without_arg of { name : string loc }
      | With_arg of
          { name : string loc
          ; arg : core_type
          }
      | Inherited of
          { core_type : core_type
          ; id : longident loc
          ; params : core_type list
          }

    let create row =
      match row.prf_desc with
      | Rtag (name, true, []) -> Ok (Without_arg { name })
      | Rtag (name, false, [ arg ]) -> Ok (With_arg { name; arg })
      | Rtag (_, false, []) -> Error "uninhabitable polymorphic variant type"
      | Rtag (_, true, _ :: _) | Rtag (_, false, _ :: _ :: _) ->
        Error "polymorphic variant intersection type"
      | Rinherit core_type ->
        (match core_type.ptyp_desc with
         | Ptyp_constr (id, params) -> Ok (Inherited { core_type; id; params })
         | _ -> Error "non-identifier inherited type")
    ;;
  end

  (** Primary recursive loop for deriving syntax from a type. *)

  let rec expand_core_type ~config ~murec core_type =
    let loc = core_type.ptyp_loc in
    let expr =
      match
        ( Attribute.get (Config.attribute_custom config) core_type
        , Attribute.get (Config.override_core_type config) core_type )
      with
      | Some expr, None ->
        (* If present, respect [@foo.custom] attributes. *)
        Config.type_of_value config ~loc (copy#core_type core_type)
        |> pexp_constraint ~loc expr
      | None, Some expr ->
        (* If present, respect [@foo.override] attributes. *)
        eapply ~loc (Config.runtime_value config ~loc "override") [ expr ]
      | Some _, Some _ ->
        (* Complain if both are present. *)
        unsupported
          ~loc
          ~config
          (Printf.sprintf
             "[@%s] and [@%s] on the same type"
             (Attribute.name (Config.attribute_custom config))
             (Attribute.name (Config.override_type_decl config)))
      | None, None ->
        (* Otherwise, follow the structure of the type. *)
        (match Ppxlib_jane.Shim.Core_type_desc.of_parsetree core_type.ptyp_desc with
         | Ptyp_var (var, _) -> evar ~loc (Config.name_of_type_variable config var)
         | Ptyp_tuple labeled_core_types ->
           (match Ppxlib_jane.as_unlabeled_tuple labeled_core_types with
            | Some core_types ->
              [ expand_tuple ~loc ~config ~murec core_types ]
              |> eapply ~loc (Config.runtime_value config ~loc "tuple")
            | None -> unsupported ~loc ~config "labeled tuple type")
         | Ptyp_constr (id, params) ->
           List.map params ~f:(expand_core_type ~config ~murec)
           |> expand_reference id ~loc ~config ~murec
         | Ptyp_alias (core_type, _, _) -> expand_core_type ~config ~murec core_type
         | Ptyp_variant (rows, _, _) ->
           [ expand_poly_variant ~loc ~config ~murec ~whole:core_type rows ]
           |> eapply ~loc (Config.runtime_value config ~loc "poly_variant")
         | desc ->
           unsupported
             ~loc
             ~config
             (Ppxlib_jane.Language_feature_name.of_core_type_desc desc))
    in
    match Attribute.get (Config.attribute_core_type config) core_type with
    | None -> expr
    | Some attr ->
      (* If present, attach [@foo] attribute expressions to derived values. *)
      eapply ~loc (Config.runtime_value config ~loc "with_attribute") [ expr; attr ]

  and expand_tuple ~loc ~config ~murec core_types =
    match
      List.mapi core_types ~f:(fun i core_type -> i, core_type) |> Binary_tree.create
    with
    | None ->
      (* The case of an empty list should never wind up here. Actual tuple types are never
         empty. Empty inline tuples in variant constructors are handled elsewhere. *)
      impossible ~loc "empty tuple"
    | Some tree ->
      let n = List.length core_types in
      let tree_expr =
        (* GADT tree, with accessors at leaves *)
        Binary_tree.fold
          tree
          ~one:(fun (i, core_type) ->
            let loc = core_type.ptyp_loc in
            [%expr
              Leaf
                { index = [%e eint ~loc i]
                ; value = [%e expand_core_type ~config ~murec core_type]
                ; access = [%e expand_tuple_get ~loc ~i ~n]
                }])
          ~two:(fun left right -> [%expr Node ([%e left], [%e right])])
      in
      let convert_expr =
        (* create function (nested pairs -> flat tuple) stored at root of tree *)
        let name = Printf.sprintf "x%i" in
        let pat =
          Binary_tree.fold
            tree
            ~one:(fun (i, _) -> pvar ~loc (name i))
            ~two:(fun left right -> ppat_tuple ~loc [ left; right ])
        in
        let expr =
          List.mapi core_types ~f:(fun i _ -> evar ~loc (name i)) |> pexp_tuple ~loc
        in
        eabstract ~loc [ pat ] expr
      in
      Some (erecord ~loc [ "tree", tree_expr; "convert", convert_expr ])
      |> pexp_construct ~loc { loc; txt = Lident "T" }

  and expand_poly_variant ~loc ~config ~murec ~whole rows =
    match Binary_tree.create rows with
    | None -> impossible ~loc "empty polymorphic variant type"
    | Some tree ->
      let tree_expr =
        (* GADT tree, with constructors at leaves *)
        Binary_tree.fold
          tree
          ~one:(expand_poly_variant_row ~config ~murec ~whole)
          ~two:(fun l r -> [%expr Node ([%e l], [%e r])])
      in
      let convert_expr =
        (* choose function (variant -> nested Either.t) stored at root of tree *)
        Binary_tree.fold
          tree
          ~one:(expand_poly_variant_choice ~config)
          ~two:(fun lefts rights ->
            List.concat
              [ List.map lefts ~f:(fun (pat, expr) -> pat, [%expr First [%e expr]])
              ; List.map rights ~f:(fun (pat, expr) -> pat, [%expr Second [%e expr]])
              ])
        |> List.map ~f:(fun (lhs, rhs) -> case ~lhs ~guard:None ~rhs)
        |> pexp_function ~loc
      in
      Some (erecord ~loc [ "tree", tree_expr; "convert", convert_expr ])
      |> pexp_construct ~loc { loc; txt = Lident "T" }

  and expand_poly_variant_row ~config ~murec ~whole row =
    let loc = row.prf_loc in
    let attribute = eoption ~loc (Attribute.get (Config.attribute_row config) row) in
    let expr =
      match Row.create row with
      | Ok (Without_arg { name }) ->
        let expr = pexp_variant ~loc name.txt None in
        [%expr
          { arg = Empty { name = [%e estring ~loc name.txt]; attribute = [%e attribute] }
          ; create = (fun () -> [%e expr])
          }]
      | Ok (With_arg { name; arg }) ->
        [%expr
          { arg =
              Value
                { name = [%e estring ~loc:name.loc name.txt]
                ; attribute = [%e attribute]
                ; value = [%e expand_core_type ~config ~murec arg]
                }
          ; create = (fun x -> [%e pexp_variant ~loc name.txt (Some [%expr x])])
          }]
      | Ok (Inherited { core_type; id = _; params = _ }) ->
        [%expr
          { arg = Inherited [%e expand_core_type ~config ~murec core_type]
          ; create =
              (fun x -> (x : [%t copy#core_type core_type] :> [%t copy#core_type whole]))
          }]
      | Error message -> unsupported ~loc ~config message
    in
    [%expr Leaf [%e expr]]

  and expand_poly_variant_choice ~config row =
    let loc = row.prf_loc in
    match Row.create row with
    | Ok (Without_arg { name }) -> [ ppat_variant ~loc name.txt None, eunit ~loc ]
    | Ok (With_arg { name; arg = _ }) ->
      [ ppat_variant ~loc name.txt (Some [%pat? x]), [%expr x] ]
    | Ok (Inherited { core_type = _; id; params = _ }) ->
      [ ppat_alias ~loc (ppat_type ~loc id) { loc; txt = "x" }, [%expr x] ]
    | Error message -> [ [%pat? _], unsupported ~loc ~config message ]
  ;;

  (** Construct the GADT "leaf" for a record type. *)
  let expand_label_declaration ~config ~murec ~as_tuple ~i ~n ld =
    let loc = ld.pld_loc in
    let name = estring ~loc:ld.pld_name.loc ld.pld_name.txt in
    let derived = expand_core_type ~config ~murec ld.pld_type in
    let get =
      match as_tuple with
      | true -> expand_tuple_get ~loc ~i ~n
      | false ->
        let pat = pvar ~loc "x" in
        pexp_field ~loc (evar ~loc "x") (Located.map_lident ld.pld_name)
        |> eabstract ~loc [ pat ]
    in
    let attribute = eoption ~loc (Attribute.get (Config.attribute_field config) ld) in
    [ "name", name; "attribute", attribute; "value", derived; "access", get ]
    |> erecord ~loc
  ;;

  (** Create function for a record type (nested pairs -> record). *)
  let expand_record_create ~loc ~as_tuple ~tree lds =
    let pat =
      Binary_tree.fold
        tree
        ~one:(fun (_, ld) -> pvar ~loc:ld.pld_name.loc ld.pld_name.txt)
        ~two:(fun l r -> [%pat? [%p l], [%p r]])
    in
    let expr =
      match as_tuple with
      | false ->
        let fields =
          List.map lds ~f:(fun ld ->
            Located.map_lident ld.pld_name, evar ~loc:ld.pld_name.loc ld.pld_name.txt)
        in
        pexp_record ~loc fields None
      | true ->
        let indices =
          List.map lds ~f:(fun ld -> evar ~loc:ld.pld_name.loc ld.pld_name.txt)
        in
        pexp_tuple_opt ~loc indices |> Option.value ~default:(eunit ~loc)
    in
    eabstract ~loc [ pat ] expr
  ;;

  (** Derived code for a record type. *)
  let expand_record ~loc ~config ~murec ~as_tuple lds =
    match List.mapi lds ~f:(fun i ld -> i, ld) |> Binary_tree.create with
    | None -> impossible ~loc "empty record"
    | Some tree ->
      let fields =
        let n = List.length lds in
        Binary_tree.fold
          tree
          ~one:(fun (i, ld) ->
            let expr = expand_label_declaration ~config ~murec ~as_tuple ~i ~n ld in
            [%expr Leaf [%e expr]])
          ~two:(fun l r -> [%expr Node ([%e l], [%e r])])
      in
      let create = expand_record_create ~loc ~as_tuple ~tree lds in
      Some (erecord ~loc [ "tree", fields; "convert", create ])
      |> pexp_construct ~loc { loc; txt = Lident "T" }
  ;;

  (** Derived constructor function for a variant clause. *)
  let expand_constructor_declaration_create ~loc cd =
    match cd.pcd_args with
    | Pcstr_tuple core_types ->
      let names = List.mapi core_types ~f:(fun i _ -> "x" ^ Int.to_string i) in
      let pat =
        List.map names ~f:(pvar ~loc)
        |> ppat_tuple_opt ~loc
        |> Option.value ~default:(punit ~loc)
      in
      let expr =
        List.map names ~f:(evar ~loc)
        |> pexp_tuple_opt ~loc
        |> pexp_construct ~loc (Located.map_lident cd.pcd_name)
      in
      eabstract ~loc [ pat ] expr
    | Pcstr_record lds ->
      let pat =
        List.map lds ~f:(fun ld -> pvar ~loc:ld.pld_name.loc ld.pld_name.txt)
        |> ppat_tuple_opt ~loc
        |> Option.value ~default:(punit ~loc)
      in
      let expr =
        let fields =
          List.map lds ~f:(fun ld ->
            Located.map_lident ld.pld_name, evar ~loc:ld.pld_name.loc ld.pld_name.txt)
        in
        Some (pexp_record ~loc fields None)
        |> pexp_construct ~loc (Located.map_lident cd.pcd_name)
      in
      eabstract ~loc [ pat ] expr
  ;;

  (** Construct the GADT "leaf" for a variant constructor. *)
  let expand_constructor_declaration ~config ~murec cd =
    let loc = cd.pcd_loc in
    match cd.pcd_res with
    | Some _ -> unsupported ~loc ~config "GADT type"
    | None ->
      let name = estring ~loc:cd.pcd_name.loc cd.pcd_name.txt in
      let args =
        match cd.pcd_args with
        | Pcstr_tuple args ->
          let core_types =
            List.map args ~f:Ppxlib_jane.Shim.Pcstr_tuple_arg.to_core_type
          in
          (match List.is_empty core_types with
           | true -> pexp_construct ~loc { loc; txt = Lident "Empty" } None
           | false ->
             Some (expand_tuple ~loc ~config ~murec core_types)
             |> pexp_construct ~loc { loc; txt = Lident "Tuple" })
        | Pcstr_record lds ->
          Some (expand_record ~loc ~config ~murec ~as_tuple:true lds)
          |> pexp_construct ~loc { loc; txt = Lident "Record" }
      in
      let create = expand_constructor_declaration_create ~loc cd in
      let attribute = eoption ~loc (Attribute.get (Config.attribute_clause config) cd) in
      [ "name", name; "attribute", attribute; "args", args; "create", create ]
      |> erecord ~loc
  ;;

  (** Pattern and body expression for choose function (variant -> nested Either.t) for a
      single constructo declaration. *)
  let expand_variant_clause_choose cd =
    let loc = cd.pcd_loc in
    let body, arg_names =
      match cd.pcd_args with
      | Pcstr_tuple core_types ->
        let arg_names =
          List.mapi core_types ~f:(fun i _ -> Printf.sprintf "__value_%i__" i)
        in
        List.map arg_names ~f:(pvar ~loc) |> ppat_tuple_opt ~loc, arg_names
      | Pcstr_record lds ->
        let arg_names =
          List.map lds ~f:(fun ld -> Printf.sprintf "__field_%s__" ld.pld_name.txt)
        in
        let fields =
          List.map2_exn arg_names lds ~f:(fun arg_name ld ->
            Located.map_lident ld.pld_name, pvar ~loc arg_name)
        in
        Some (ppat_record ~loc fields Closed), arg_names
    in
    [ ( ppat_construct ~loc (Located.map_lident cd.pcd_name) body
      , pexp_tuple_opt ~loc (List.map arg_names ~f:(evar ~loc))
        |> Option.value ~default:(eunit ~loc) )
    ]
  ;;

  let expand_variant ~loc ~config ~murec td cds =
    match Binary_tree.create cds with
    | None ->
      (* empty type *)
      let self_type =
        List.map td.ptype_params ~f:fst
        |> ptyp_constr ~loc (Located.map_lident td.ptype_name)
      in
      [ [ case
            ~lhs:(ppat_constraint ~loc (ppat_any ~loc) self_type)
            ~guard:None
            ~rhs:(pexp_unreachable ~loc)
        ]
        |> pexp_function ~loc
      ]
      |> eapply ~loc (Config.runtime_value config ~loc "empty")
    | Some tree ->
      (* non-empty variant type *)
      let tree_expr =
        (* GADT tree with variant constructors at the leaves *)
        Binary_tree.fold
          tree
          ~one:(fun cd ->
            [%expr Leaf [%e expand_constructor_declaration ~config ~murec cd]])
          ~two:(fun l r -> [%expr Node ([%e l], [%e r])])
      in
      let convert_expr =
        (* choose function (variant -> nested Either.t) stored at root of tree *)
        Binary_tree.fold tree ~one:expand_variant_clause_choose ~two:(fun lefts rights ->
          List.concat
            [ List.map lefts ~f:(fun (pat, expr) -> pat, [%expr First [%e expr]])
            ; List.map rights ~f:(fun (pat, expr) -> pat, [%expr Second [%e expr]])
            ])
        |> List.map ~f:(fun (lhs, rhs) -> case ~lhs ~guard:None ~rhs)
        |> pexp_function ~loc
      in
      Some (erecord ~loc [ "tree", tree_expr; "convert", convert_expr ])
      |> pexp_construct ~loc { loc; txt = Lident "T" }
      |> List.return
      |> eapply ~loc (Config.runtime_value config ~loc "variant")
  ;;

  (** Expansion for a type declaration. Takes a set of type names being defined mutually
      recursively with it. *)
  let expand_type_declaration ~config ~murec td =
    let loc = td.ptype_loc in
    let body =
      match Attribute.get (Config.override_type_decl config) td with
      | Some expr -> eapply ~loc (Config.runtime_value config ~loc "override") [ expr ]
      | None ->
        (match Ppxlib_jane.Shim.Type_kind.of_parsetree td.ptype_kind with
         | Ptype_open -> unsupported ~loc ~config "open type"
         | Ptype_record lds ->
           [ expand_record ~loc ~config ~murec ~as_tuple:false lds ]
           |> eapply ~loc (Config.runtime_value config ~loc "record")
         | Ptype_record_unboxed_product _ ->
           unsupported ~loc ~config "unboxed record type"
         | Ptype_variant cds -> expand_variant ~loc ~config ~murec td cds
         | Ptype_abstract ->
           (match td.ptype_manifest with
            | None -> unsupported ~loc ~config "abstract type"
            | Some core_type -> expand_core_type ~config ~murec core_type))
    in
    let body =
      match Attribute.get (Config.attribute_type_decl config) td with
      | None -> body
      | Some expr ->
        eapply ~loc (Config.runtime_value config ~loc "with_attribute") [ body; expr ]
    in
    let body = wrap_runtime_error ~loc body in
    let body = if Set.is_empty murec then body else pexp_lazy ~loc body in
    let params =
      List.map td.ptype_params ~f:(fun param ->
        Config.name_of_type_variable config (get_type_param_name param).txt
        |> pvar ~loc:(fst param).ptyp_loc)
    in
    eabstract ~loc params body
  ;;

  (** Expand straightforward nonrecursive definitions. *)
  let expand_nonrecursive ~loc ~config tds =
    let murec = Set.empty (module String) in
    List.map tds ~f:(fun td ->
      let pat = pvar ~loc (Config.name_of_value config td.ptype_name.txt) in
      let expr = expand_type_declaration ~config ~murec td in
      value_binding ~loc ~pat ~expr)
    |> pstr_value_list ~loc Nonrecursive
  ;;

  (** Expand mutually recursive definitions via [lazy] indirection. *)
  let expand_recursive ~loc ~config tds =
    let murec =
      List.map tds ~f:(fun td -> td.ptype_name.txt) |> Set.of_list (module String)
    in
    let pat =
      List.map tds ~f:(fun td ->
        pvar ~loc (Config.name_of_value config td.ptype_name.txt))
      |> ppat_tuple_opt ~loc
      |> Option.value ~default:(punit ~loc)
    in
    let expr =
      let binds =
        List.map tds ~f:(fun td ->
          let pat = pvar ~loc (Config.name_of_lazy_value config td.ptype_name.txt) in
          let expr = expand_type_declaration ~config ~murec td in
          value_binding ~loc ~pat ~expr)
      in
      List.map tds ~f:(fun td ->
        let loc = td.ptype_loc in
        let params = List.map td.ptype_params ~f:get_type_param_name in
        [ eapply
            ~loc
            (evar ~loc (Config.name_of_lazy_value config td.ptype_name.txt))
            (List.map params ~f:(fun name -> evar ~loc:name.loc name.txt))
        ]
        |> eapply ~loc [%expr Stdlib.Lazy.force]
        |> eabstract ~loc (List.map params ~f:(fun name -> pvar ~loc:name.loc name.txt)))
      |> pexp_tuple_opt ~loc
      |> Option.value ~default:(eunit ~loc)
      |> pexp_let ~loc Recursive binds
    in
    [ value_binding ~loc ~pat ~expr ] |> pstr_value_list ~loc Nonrecursive
  ;;

  (** Derive code from a set of type definitions. *)
  let expand ~config ~loc ~path:_ (rec_flag, tds) : structure =
    let tds = List.map tds ~f:name_type_params_in_td in
    let rec_flag = really_recursive rec_flag tds in
    let signature_check =
      (* Add a check that the runtime module satisfies the right signature. This should
         give a better type error by failing fast on missing or mistyped names, rather
         than waiting to discover such errors by expanding a type that happens to use the
         offending features.*)
      let name = { loc; txt = None } in
      let expr =
        pmod_constraint
          ~loc
          (pmod_ident ~loc { loc; txt = Config.module_id config })
          (pmty_ident ~loc { loc; txt = Ldot (Lident "Ppx_derive_at_runtime_lib", "S") })
      in
      pstr_module ~loc (module_binding ~loc ~name ~expr)
    in
    let definitions =
      match rec_flag with
      | Nonrecursive -> expand_nonrecursive ~loc ~config tds
      | Recursive -> expand_recursive ~loc ~config tds
    in
    signature_check :: definitions
  ;;

  (** Expand [%foo: _] extensions. *)
  let extension ~config ~loc ~path:_ core_type : expression =
    let loc = { loc with loc_ghost = true } in
    pexp_constraint
      ~loc
      (expand_core_type
         ~config
         ~murec:(Set.empty (module String))
         (copy#core_type core_type))
      (Config.type_of_value config ~loc core_type)
    |> wrap_runtime_error ~loc
  ;;
end

(** Create a new [@@deriving] for a runtime module. *)
let register_fully_qualified_runtime_module here module_path =
  let config = Config.create ~here ~module_path in
  let sig_type_decl = Deriving.Generator.make_noarg (Signature.expand ~config) in
  let str_type_decl = Deriving.Generator.make_noarg (Structure.expand ~config) in
  let extension = Structure.extension ~config in
  Driver.register_transformation
    (Config.name_of_deriving config)
    ~rules:[ Context_free.Rule.extension (Signature.extension ~config) ];
  Config.name_of_deriving config |> Deriving.add ~sig_type_decl ~str_type_decl ~extension
;;
