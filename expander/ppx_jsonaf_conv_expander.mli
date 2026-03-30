open Ppxlib

module Attrs : sig
  val default : (label_declaration, expression) Attribute.t
  val drop_default : (label_declaration, expression option) Attribute.t
  val drop_if : (label_declaration, expression) Attribute.t
end

module Config : sig
  type t

  val default : t
  val stack : t
end

module Jsonaf_of : sig
  val type_extension : core_type -> core_type
  val core_type : core_type -> expression

  val sig_type_decl
    :  loc:Location.t
    -> path:string
    -> rec_flag * type_declaration list
    -> signature_item list

  val str_type_decl
    :  loc:Location.t
    -> path:string
    -> rec_flag * type_declaration list
    -> Capitalization_ppx_configuration.t option
    -> structure
end

module Jsonaf_fields : sig
  val str_type_decl
    :  loc:Location.t
    -> path:string
    -> rec_flag * type_declaration list
    -> Capitalization_ppx_configuration.t option
    -> structure
end

module Of_jsonaf : sig
  val type_extension : config:Config.t -> core_type -> core_type
  val core_type : config:Config.t -> path:string -> core_type -> expression

  val sig_type_decl
    :  config:Config.t
    -> poly:bool
    -> loc:Location.t
    -> path:string
    -> rec_flag * type_declaration list
    -> signature_item list

  val str_type_decl
    :  config:Config.t
    -> loc:Location.t
    -> poly:bool
    -> path:string
    -> rec_flag * type_declaration list
    -> Capitalization_ppx_configuration.t option
    -> structure
end

module Sig_jsonaf : sig
  val sig_type_decl
    :  loc:Location.t
    -> path:string
    -> rec_flag * type_declaration list
    -> signature_item list
end
