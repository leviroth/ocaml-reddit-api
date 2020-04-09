open! Core

module Id : sig
  type t =
    { subreddit : Subreddit_name.t option
    ; page : string
    }
  [@@deriving sexp]
end

type t [@@deriving sexp]

val of_json : Json.t -> t Or_error.t
val to_json : t -> Json.t
val may_revise : t -> bool Or_error.t
val revision_id : t -> Uuid.t Or_error.t
val revision_by : t -> Thing.user Or_error.t
val content : t -> [ `markdown | `HTML ] -> string Or_error.t
val revision_time : t -> Time_ns.t Or_error.t
val revision_reason : t -> string option Or_error.t
