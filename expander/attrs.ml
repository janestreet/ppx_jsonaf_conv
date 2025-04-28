open! Base
open! Ppxlib

let default =
  Attribute.declare
    "jsonaf.default"
    Attribute.Context.label_declaration
    Ast_pattern.(pstr (pstr_eval __ nil ^:: nil))
    (fun x -> x)
;;

let drop_default =
  Attribute.declare
    "jsonaf.jsonaf_drop_default"
    Attribute.Context.label_declaration
    Ast_pattern.(pstr (alt_option (pstr_eval __ nil ^:: nil) nil))
    (fun x -> x)
;;

let drop_default_equal =
  Attribute.declare
    "jsonaf.@jsonaf_drop_default.equal"
    Attribute.Context.label_declaration
    Ast_pattern.(pstr nil)
    ()
;;

let drop_default_compare =
  Attribute.declare
    "jsonaf.@jsonaf_drop_default.compare"
    Attribute.Context.label_declaration
    Ast_pattern.(pstr nil)
    ()
;;

let drop_default_jsonaf =
  Attribute.declare
    "jsonaf.@jsonaf_drop_default.jsonaf"
    Attribute.Context.label_declaration
    Ast_pattern.(pstr nil)
    ()
;;

let drop_if =
  Attribute.declare
    "jsonaf.jsonaf_drop_if"
    Attribute.Context.label_declaration
    Ast_pattern.(pstr (pstr_eval __ nil ^:: nil))
    (fun x -> x)
;;

let opaque =
  Attribute.declare "jsonaf.opaque" Attribute.Context.core_type Ast_pattern.(pstr nil) ()
;;

let option =
  Attribute.declare
    "jsonaf.option"
    Attribute.Context.label_declaration
    Ast_pattern.(pstr nil)
    ()
;;

let list =
  Attribute.declare
    "jsonaf.list"
    Attribute.Context.label_declaration
    Ast_pattern.(pstr nil)
    ()
;;

let allow_extra_fields_td =
  Attribute.declare
    "jsonaf.allow_extra_fields"
    Attribute.Context.type_declaration
    Ast_pattern.(pstr nil)
    ()
;;

let allow_extra_fields_cd =
  Attribute.declare
    "jsonaf.allow_extra_fields"
    Attribute.Context.constructor_declaration
    Ast_pattern.(pstr nil)
    ()
;;

let allow_extra_fields_log_td =
  Attribute.declare
    "jsonaf.allow_extra_fields.log"
    Attribute.Context.type_declaration
    Ast_pattern.(pstr nil)
    ()
;;

let allow_extra_fields_log_cd =
  Attribute.declare
    "jsonaf.allow_extra_fields.log"
    Attribute.Context.constructor_declaration
    Ast_pattern.(pstr nil)
    ()
;;

let jsonaf_key =
  Attribute.declare
    "jsonaf.key"
    Attribute.Context.label_declaration
    Ast_pattern.(pstr (pstr_eval (estring __) nil ^:: nil))
    (fun x -> x)
;;

let jsonaf_variant_name =
  Attribute.declare
    "jsonaf.name"
    Attribute.Context.constructor_declaration
    Ast_pattern.(pstr (pstr_eval (estring __) nil ^:: nil))
    (fun x -> x)
;;

let jsonaf_polymorphic_variant_name =
  Attribute.declare
    "jsonaf.name"
    Attribute.Context.rtag
    Ast_pattern.(pstr (pstr_eval (estring __) nil ^:: nil))
    (fun x -> x)
;;

let invalid_attribute ~loc attr description =
  Location.raise_errorf
    ~loc
    "ppx_jsonaf_conv: [@%s] is only allowed on type [%s]."
    (Attribute.name attr)
    description
;;

let fail_if_allow_extra_field_cd ~loc x =
  if Option.is_some (Attribute.get allow_extra_fields_cd x)
     || Option.is_some (Attribute.get allow_extra_fields_log_cd x)
  then
    Location.raise_errorf
      ~loc
      "ppx_jsonaf_conv: [@@allow_extra_fields] is only allowed on inline records."
;;

let fail_if_allow_extra_field_td ~loc x =
  match
    Attribute.get allow_extra_fields_td x, Attribute.get allow_extra_fields_log_td x
  with
  | None, None -> ()
  | Some (), Some () ->
    Location.raise_errorf
      ~loc
      "ppx_jsonaf_conv: You can only specify one of [@@@@allow_extra_fields] and \
       [@@@@allow_extra_fields.log]"
  | Some (), None | None, Some () ->
    (match x.ptype_kind with
     | Ptype_variant cds
       when List.exists cds ~f:(fun cd ->
              match cd.pcd_args with
              | Pcstr_record _ -> true
              | _ -> false) ->
       Location.raise_errorf
         ~loc
         "ppx_jsonaf_conv: [@@@@allow_extra_fields] only works on records. For inline \
          records, do: type t = A of { a : int } [@@allow_extra_fields] | B \
          [@@@@deriving jsonaf]"
     | _ ->
       Location.raise_errorf
         ~loc
         "ppx_jsonaf_conv: [@@@@allow_extra_fields] is only allowed on records.")
;;

module Record_field_handler = struct
  type common =
    [ `jsonaf_option of core_type
    | `jsonaf_list
    ]

  let get_attribute attr ld ~f =
    Option.map (Attribute.get attr ld) ~f:(fun x -> f x, Attribute.name attr)
  ;;

  let create ~loc getters ld =
    let common_getters =
      [ (fun ld ->
          match Attribute.get option ld with
          | Some () ->
            (match ld.pld_type with
             | [%type: [%t? ty] option] -> Some (`jsonaf_option ty, "[@jsonaf.option]")
             | _ -> invalid_attribute ~loc option "_ option")
          | None -> None)
      ; (fun ld ->
          match Attribute.get list ld with
          | Some () ->
            (match ld.pld_type with
             | [%type: [%t? _] list] -> Some (`jsonaf_list, "[@jsonaf.list]")
             | _ -> invalid_attribute ~loc list "_ list")
          | None -> None)
      ]
    in
    match List.filter_map (getters @ common_getters) ~f:(fun f -> f ld) with
    | [] -> None
    | [ (v, _) ] -> Some v
    | _ :: _ :: _ as attributes ->
      Location.raise_errorf
        ~loc
        "The following elements are mutually exclusive: %s"
        (String.concat ~sep:" " (List.map attributes ~f:snd))
  ;;

  module Of_jsonaf = struct
    type t =
      [ common
      | `default of expression
      ]

    let create ~loc ld =
      create ~loc [ get_attribute default ~f:(fun default -> `default default) ] ld
    ;;
  end

  module Jsonaf_of = struct
    type t =
      [ common
      | `drop_default of [ `no_arg | `compare | `equal | `jsonaf | `func of expression ]
      | `drop_if of expression
      | `keep
      ]

    let create ~loc ld =
      create
        ~loc
        [ get_attribute drop_default ~f:(function
            | None -> `drop_default `no_arg
            | Some e -> `drop_default (`func e))
        ; get_attribute drop_default_equal ~f:(fun () -> `drop_default `equal)
        ; get_attribute drop_default_compare ~f:(fun () -> `drop_default `compare)
        ; get_attribute drop_default_jsonaf ~f:(fun () -> `drop_default `jsonaf)
        ; get_attribute drop_if ~f:(fun x -> `drop_if x)
        ]
        ld
      |> Option.value ~default:`keep
    ;;
  end
end
