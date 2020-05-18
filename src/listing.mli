open! Core

module Page_id : sig
  type t [@@deriving sexp]

  include Stringable with type t := t
end

type 'child t [@@deriving sexp]

val of_json : (Json.t -> 'child) -> Json.t -> 'child t
val children : 'child t -> 'child list
val after : _ t -> Page_id.t option
