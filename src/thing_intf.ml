open! Core

module type Common = sig
  type t [@@deriving sexp]

  val of_json : Yojson.Safe.t -> t
  val to_json : t -> Yojson.Safe.t
  val fullname : t -> Fullname.t option
  val get_field : t -> string -> Json_derivers.Yojson.t option
end

module type Thing = sig
  module rec Comment : sig
    include Common

    val author : t -> Username.t option
    val moderation_info : t -> Moderation_info.t option
  end

  and User : sig
    include Common
  end

  and Link : sig
    include Common

    val author : t -> Username.t option
    val moderation_info : t -> Moderation_info.t option
  end

  and Message : Common
  and Subreddit : Common
  and Award : Common

  type t =
    | Comment of Comment.t
    | User of User.t
    | Link of Link.t
    | Message of Message.t
    | Subreddit of Subreddit.t
    | Award of Award.t

  include Common with type t := t
end
