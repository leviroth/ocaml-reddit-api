open! Core_kernel

include module type of struct
  include Ezjsonm
end

type t = value [@@deriving sexp, bin_io]

val of_string : string -> t Or_error.t
val get_map : t -> t Map.M(String).t
