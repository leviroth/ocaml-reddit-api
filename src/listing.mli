open! Core

module Page_id : sig
  type t [@@deriving sexp]

  include Stringable with type t := t

  val of_fullname : Thing.Fullname.t -> t
  val to_fullname : t -> Thing.Fullname.t option
end

type 'child t [@@deriving sexp]

val of_json : (Yojson.Safe.t -> 'child Or_error.t) -> Yojson.Safe.t -> 'child t Or_error.t
val children : 'child t -> 'child list
val after : _ t -> Page_id.t option
