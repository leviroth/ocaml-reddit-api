open! Core

module Page_id : sig
  type t [@@deriving sexp]

  include Stringable with type t := t

  val of_fullname : Thing.Fullname.t -> t
  val to_fullname : t -> Thing.Fullname.t option
end

type 'child t [@@deriving sexp]

val of_json : (Json.t -> 'child) -> Json.t -> 'child t
val children : 'child t -> 'child list
val after : _ t -> Page_id.t option
