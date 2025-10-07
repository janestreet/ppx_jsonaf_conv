(* jsonaf_conv: Preprocessing Module for Automated Jsonaf_kernel Conversions *)

open Ppxlib
module Attrs = Ppx_jsonaf_conv_expander.Attrs

let capitalization_arg =
  Capitalization_ppx_configuration.argument ~ppx_name:"ppx_jsonaf_conv"
;;

module Jsonaf_of = struct
  module E = Ppx_jsonaf_conv_expander.Jsonaf_of

  let name = "jsonaf_of"

  let str_type_decl =
    Deriving.Generator.make
      Deriving.Args.(empty +> capitalization_arg)
      E.str_type_decl
      ~attributes:
        [ Attribute.T Attrs.default
        ; Attribute.T Attrs.drop_default
        ; Attribute.T Attrs.drop_if
        ]
  ;;

  let sig_type_decl = Deriving.Generator.make_noarg E.sig_type_decl
  let extension ~loc:_ ~path:_ ctyp = E.core_type ctyp
  let deriver = Deriving.add name ~str_type_decl ~sig_type_decl ~extension

  let () =
    Driver.register_transformation
      name
      ~rules:
        [ Context_free.Rule.extension
            (Extension.declare
               name
               Core_type
               Ast_pattern.(ptyp __)
               (fun ~loc:_ ~path:_ ty -> E.type_extension ty))
        ]
  ;;
end

module Jsonaf_fields = struct
  module E = Ppx_jsonaf_conv_expander.Jsonaf_fields

  let name = "jsonaf_fields"

  let str_type_decl =
    Deriving.Generator.make
      Deriving.Args.(empty +> capitalization_arg)
      E.str_type_decl
      ~attributes:[]
  ;;

  let deriver = Deriving.add name ~str_type_decl
end

module Of_jsonaf = struct
  module E = Ppx_jsonaf_conv_expander.Of_jsonaf

  let name = "of_jsonaf"

  let str_type_decl =
    Deriving.Generator.make
      Deriving.Args.(empty +> capitalization_arg)
      (E.str_type_decl ~poly:false)
      ~attributes:[ Attribute.T Attrs.default ]
  ;;

  let sig_type_decl = Deriving.Generator.make_noarg (E.sig_type_decl ~poly:false)
  let extension ~loc:_ ~path ctyp = E.core_type ~path ctyp
  let deriver = Deriving.add name ~str_type_decl ~sig_type_decl ~extension

  let () =
    Driver.register_transformation
      name
      ~rules:
        [ Context_free.Rule.extension
            (Extension.declare
               name
               Core_type
               Ast_pattern.(ptyp __)
               (fun ~loc:_ ~path:_ ty -> E.type_extension ty))
        ]
  ;;
end

module Of_jsonaf_poly = struct
  module E = Ppx_jsonaf_conv_expander.Of_jsonaf

  let str_type_decl =
    Deriving.Generator.make
      Deriving.Args.(empty +> capitalization_arg)
      (E.str_type_decl ~poly:true)
      ~attributes:[ Attribute.T Attrs.default ]
  ;;

  let sig_type_decl = Deriving.Generator.make_noarg (E.sig_type_decl ~poly:true)
  let deriver = Deriving.add "of_jsonaf_poly" ~sig_type_decl ~str_type_decl
end

let jsonaf_of = Jsonaf_of.deriver
let jsonaf_fields_of = Jsonaf_fields.deriver
let of_jsonaf = Of_jsonaf.deriver
let of_jsonaf_poly = Of_jsonaf_poly.deriver

module Jsonaf_in_sig = struct
  module E = Ppx_jsonaf_conv_expander.Sig_jsonaf

  let sig_type_decl = Deriving.Generator.make_noarg E.sig_type_decl

  let deriver =
    Deriving.add
      "ppx_jsonaf_conv: let this be a string that wouldn't parse if put in the source"
      ~sig_type_decl
  ;;
end

let jsonaf =
  Deriving.add_alias
    "jsonaf"
    [ jsonaf_of; of_jsonaf ]
    ~sig_type_decl:[ Jsonaf_in_sig.deriver ]
;;

let jsonaf_poly = Deriving.add_alias "jsonaf_poly" [ jsonaf_of; of_jsonaf_poly ]
