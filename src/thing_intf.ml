open! Core

module type Common = sig
  type t [@@deriving sexp]

  val of_json : Yojson.Safe.t -> t
  val to_json : t -> Yojson.Safe.t
  val fullname : t -> Fullname.t option
  val get_field : t -> string -> Json_derivers.Yojson.t option
end

module type Thing = sig
  module rec User : sig
    include Common
  end

  and Link : sig
    include Common

    val author : t -> Username.t option
    val moderation_info : t -> Moderation_info.t option
  end

  and Comment : sig
    include Common

    val author : t -> Username.t option
    val moderation_info : t -> Moderation_info.t option
  end

  include Common
end
