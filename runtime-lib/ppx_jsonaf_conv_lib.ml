module Jsonaf_conv_error = Jsonaf_conv_error
module Jsonaf_conv = Jsonaf_kernel.Conv
module Jsonafable = Jsonaf_kernel.Jsonafable
module Jsonaf_kernel = Jsonaf_kernel

module Option = struct
  type 'a t = 'a option =
    | None
    | Some of 'a
end

external ignore : _ -> unit = "%ignore"
external poly_equal : 'a -> 'a -> bool = "%equal"

let ( ! ) : 'a ref -> 'a = fun x -> !x
