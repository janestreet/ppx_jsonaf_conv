module Jsonaf_conv_error = Jsonaf_conv_error
module Jsonaf_conv = Jsonaf_kernel.Conv
module Jsonafable = Jsonaf_kernel.Jsonafable
module Jsonaf_kernel = Jsonaf_kernel

(** Returns the current value of [Jsonaf_kernel.Conv.record_check_extra_fields].

    When [true], the generated [of_jsonaf] functions for records will track any extra
    fields present in the JSON that don't correspond to record fields. *)
val check_extra_record_fields : unit -> bool

(** {2 Re-exported values}
    We export these to protect the ppx expansion from user code shadowing names *)

module Option : sig
  type ('a : value_or_null) t = 'a option =
    | None
    | Some of 'a

  val is_none : ('a : value_or_null). 'a option @ local -> bool
end

module Or_null : sig
  type 'a t = 'a Basement.Or_null_shim.t
end

external ignore : _ -> unit = "%ignore"
external poly_equal : 'a -> 'a -> bool = "%equal"
val ( ! ) : ('a : value_or_null). 'a ref -> 'a
