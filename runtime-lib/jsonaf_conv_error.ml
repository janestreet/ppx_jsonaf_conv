(* Conv_error: Module for Handling Errors during Automated Jsonaf_kernel Conversions *)

open! StdLabels
open Jsonaf_kernel.Conv

let sprintf = Printf.sprintf

exception No_variant_match

[%%template
external maybe_globalize_string : string -> string = "caml_obj_dup" [@@mode local]

let[@mode global] maybe_globalize_string s = s

[@@@mode.default m = (global, local)]

(* Errors concerning tuples *)

let tuple_of_size_n_expected loc n jsonaf =
  (of_jsonaf_error [@mode m])
    (sprintf "%s_of_jsonaf: tuple of size %d expected" loc n)
    jsonaf
;;

(* Errors concerning sum types *)

let stag_no_args loc jsonaf =
  (of_jsonaf_error [@mode m])
    (loc ^ "_of_jsonaf: this constructor does not take arguments")
    jsonaf
;;

let stag_incorrect_n_args loc tag jsonaf =
  let msg =
    sprintf
      "%s_of_jsonaf: sum tag %S has incorrect number of arguments"
      loc
      ((maybe_globalize_string [@mode m]) tag)
  in
  (of_jsonaf_error [@mode m]) msg jsonaf
;;

let stag_takes_args loc jsonaf =
  (of_jsonaf_error [@mode m])
    (loc ^ "_of_jsonaf: this constructor requires arguments")
    jsonaf
;;

let nested_list_invalid_sum loc jsonaf =
  (of_jsonaf_error [@mode m])
    (loc ^ "_of_jsonaf: expected a variant type, saw a nested list")
    jsonaf
;;

let empty_list_invalid_sum loc jsonaf =
  (of_jsonaf_error [@mode m])
    (loc ^ "_of_jsonaf: expected a variant type, saw an empty list")
    jsonaf
;;

let unexpected_stag loc jsonaf =
  (of_jsonaf_error [@mode m]) (loc ^ "_of_jsonaf: unexpected variant constructor") jsonaf
;;

(* Errors concerning records *)

let globalize_and_reverse items =
  let rec loop acc rest =
    match rest with
    | [] -> acc
    | x :: xs -> loop ((maybe_globalize_string [@mode m]) x :: acc) xs
  in
  loop [] items
;;

let format_superfluous_fields ~what ~loc rev_fld_names =
  let fld_names = (globalize_and_reverse [@mode m]) rev_fld_names in
  let fld_names_str = String.concat fld_names ~sep:" " in
  sprintf "%s_of_jsonaf: %s: %s" loc what fld_names_str
;;

let record_superfluous_fields ~what ~loc rev_fld_names jsonaf =
  let msg = (format_superfluous_fields [@mode m]) ~what ~loc rev_fld_names in
  (of_jsonaf_error [@mode m]) msg jsonaf
;;

let record_duplicate_fields loc rev_fld_names jsonaf =
  (record_superfluous_fields [@mode m]) ~what:"duplicate fields" ~loc rev_fld_names jsonaf
;;

let record_extra_fields loc rev_fld_names jsonaf =
  (record_superfluous_fields [@mode m]) ~what:"extra fields" ~loc rev_fld_names jsonaf
;;

let collect_undefined_fields maybe_fields =
  let[@tail_mod_cons] rec loop = function
    | [] -> []
    | (true, field) :: rest -> (maybe_globalize_string [@mode m]) field :: loop rest
    | (false, _) :: rest -> loop rest
  in
  String.concat (loop maybe_fields) ~sep:" "
;;

let record_undefined_elements loc jsonaf lst =
  let undefined = (collect_undefined_fields [@mode m]) lst in
  let msg =
    sprintf "%s_of_jsonaf: the following record elements were undefined: %s" loc undefined
  in
  (of_jsonaf_error [@mode m]) msg jsonaf
;;

let record_list_instead_atom loc jsonaf =
  let msg = loc ^ "_of_jsonaf: list instead of atom for record expected" in
  (of_jsonaf_error [@mode m]) msg jsonaf
;;

let record_poly_field_value loc jsonaf =
  let msg =
    loc
    ^ "_of_jsonaf: cannot convert values of types resulting from polymorphic record \
       fields"
  in
  (of_jsonaf_error [@mode m]) msg jsonaf
;;

(* Errors concerning polymorphic variants *)

let no_variant_match () = raise No_variant_match

let no_matching_variant_found loc jsonaf =
  (of_jsonaf_error [@mode m]) (loc ^ "_of_jsonaf: no matching variant found") jsonaf
;;

let ptag_no_args loc jsonaf =
  (of_jsonaf_error [@mode m])
    (loc ^ "_of_jsonaf: polymorphic variant does not take arguments")
    jsonaf
;;

let ptag_incorrect_n_args loc cnstr jsonaf =
  let msg =
    sprintf
      "%s_of_jsonaf: polymorphic variant tag %S has incorrect number of arguments"
      loc
      ((maybe_globalize_string [@mode m]) cnstr)
  in
  (of_jsonaf_error [@mode m]) msg jsonaf
;;

let ptag_takes_args loc jsonaf =
  (of_jsonaf_error [@mode m])
    (loc ^ "_of_jsonaf: polymorphic variant tag takes an argument")
    jsonaf
;;

let nested_list_invalid_poly_var loc jsonaf =
  (of_jsonaf_error [@mode m])
    (loc ^ "_of_jsonaf: a nested list is an invalid polymorphic variant")
    jsonaf
;;

let empty_list_invalid_poly_var loc jsonaf =
  (of_jsonaf_error [@mode m])
    (loc ^ "_of_jsonaf: the empty list is an invalid polymorphic variant")
    jsonaf
;;

let empty_type loc jsonaf =
  (of_jsonaf_error [@mode m]) (loc ^ "_of_jsonaf: trying to convert an empty type") jsonaf
;;]
