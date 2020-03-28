open! Core

module type Common = sig
  type t [@@deriving sexp]

  module Id36 : Id36.S

  val of_json : Yojson.Safe.t -> t
  val to_json : t -> Yojson.Safe.t
  val get_field : t -> string -> Json_derivers.Yojson.t option
  val id36 : t -> Id36.t option
end

module Projectors = struct
  module Ident (M : Common) = M
  module Id36 (M : Common) = M.Id36
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
  and More_comments : Common
  and Modmail_conversation : Common

  module type Per_kind = sig
    module F (M : Common) : T

    type comment = [ `Comment of F(Comment).t ] [@@deriving sexp]
    type user = [ `User of F(User).t ] [@@deriving sexp]
    type link = [ `Link of F(Link).t ] [@@deriving sexp]
    type message = [ `Message of F(Message).t ] [@@deriving sexp]
    type subreddit = [ `Subreddit of F(Subreddit).t ] [@@deriving sexp]
    type award = [ `Award of F(Award).t ] [@@deriving sexp]
    type more_comments = [ `More_comments of F(More_comments).t ] [@@deriving sexp]

    type modmail_conversation = [ `Modmail_conversation of F(Modmail_conversation).t ]
    [@@deriving sexp]

    type t =
      [ comment
      | user
      | link
      | message
      | subreddit
      | award
      | more_comments
      | modmail_conversation
      ]
    [@@deriving sexp]
  end

  include Per_kind with module F := Projectors.Ident

  val of_json : Yojson.Safe.t -> t
  val to_json : t -> Yojson.Safe.t
  val get_field : t -> string -> Json_derivers.Yojson.t option

  module Fullname : sig
    include Per_kind with module F := Projectors.Id36

    val to_string : [< t ] -> string
    val of_string : string -> [> t ]
  end
end
