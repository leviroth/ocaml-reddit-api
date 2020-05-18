open! Core

module type S = sig
  type t [@@deriving sexp]

  module Id : Id36.S

  val of_json : Json.t -> t
  val of_json_with_tag_exn : Json.t -> t
  val to_json : t -> Json.t
  val get_field : t -> string -> Json.t option
  val get_field_exn : t -> string -> Json.t
  val id : t -> Id.t
end

module type Thing = sig
  module type S = S

  module rec Comment : sig
    include S

    val author : t -> Username.t
    val moderation_info : t -> Moderation_info.t option
  end

  and User : sig
    include S
  end

  and Link : sig
    include S

    val author : t -> Username.t
    val moderation_info : t -> Moderation_info.t option
  end

  and Message : sig
    include S

    val author : t -> Username.t
  end

  and Subreddit : sig
    include S
  end

  and Award : sig
    include S
  end

  and More_comments : sig
    include S
  end

  and Modmail_conversation : sig
    include S
  end

  module Poly : sig
    type t =
      [ `Comment of Comment.t
      | `User of User.t
      | `Link of Link.t
      | `Message of Message.t
      | `Subreddit of Subreddit.t
      | `Award of Award.t
      | `More_comments of More_comments.t
      | `Modmail_conversation of Modmail_conversation.t
      ]
    [@@deriving sexp]

    val of_json : Json.t -> [> t ]
  end
end
