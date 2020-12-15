open! Core_kernel

module type S = sig
  type t [@@deriving sexp]

  include Identifiable.S with type t := t

  val of_bigint : Bigint.t -> t
  val to_bigint : t -> Bigint.t
end

module type Id36 = sig
  module type S = S

  include S
end
