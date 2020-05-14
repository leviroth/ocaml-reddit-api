open! Core

module Id : sig
  type t =
    { subreddit : Subreddit_name.t option
    ; page : string
    }
  [@@deriving sexp]
end

type t [@@deriving sexp]

val of_json : Json.t -> t
val to_json : t -> Json.t
val may_revise : t -> bool
val revision_id : t -> Uuid.t
val revision_by : t -> Thing.user
val content : t -> [ `markdown | `HTML ] -> string
val revision_time : t -> Time_ns.t
val revision_reason : t -> string option

module Edit_conflict : sig
  type t [@@deriving sexp]

  val of_json : Json.t -> t
  val diff : t -> string
  val message : t -> string
  val new_content : t -> string
  val new_revision : t -> Uuid.t
  val reason : t -> string
end
