open! Core

module type S = sig
  type t [@@deriving sexp]

  include Stringable.S with type t := t

  val of_int : int -> t
  val to_int : t -> int
end

module type Id36 = sig
  module Comment : S
  module User : S
  module Link : S
  module Message : S
  module Subreddit : S
  module Award : S
  module Modmail_conversation : S
  module More_children : S
end
