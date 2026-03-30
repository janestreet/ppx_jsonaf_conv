module Jsonaf_conv_error = Jsonaf_conv_error
module Jsonaf_conv = Jsonaf_kernel.Conv
module Jsonafable = Jsonaf_kernel.Jsonafable
module Jsonaf_kernel = Jsonaf_kernel

module Option = struct
  type 'a t = 'a option =
    | None
    | Some of 'a

  let is_none = function
    | None -> true
    | Some _ -> false
  ;;
end

module Or_null = struct
  type 'a t = 'a Basement.Or_null_shim.t
end

external ignore : _ -> unit = "%ignore"
external poly_equal : 'a -> 'a -> bool = "%equal"

let ( ! ) : 'a ref -> 'a = fun x -> !x
let[@inline] check_extra_record_fields () = !Jsonaf_conv.record_check_extra_fields
