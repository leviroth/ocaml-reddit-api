open! Core_kernel

module type S = sig
  type t [@@deriving sexp]

  include Identifiable.S with type t := t

  val of_int : int -> t
  val to_int : t -> int
end

module type Id36 = sig
  module type S = S

  include S
end
