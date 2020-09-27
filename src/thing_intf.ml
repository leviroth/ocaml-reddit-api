open! Core

module type S = sig
  type t [@@deriving sexp, bin_io]

  module Id : Id36.S
  include Jsonable.S with type t := t

  val get_field : t -> string -> Json.t option
  val get_field_exn : t -> string -> Json.t
  val id : t -> Id.t
  val field_map : t -> Json.t String.Map.t
end

module type Thing = sig
  module type S = S

  module rec Comment : sig
    include S

    module Score : sig
      type t =
        | Score of int
        | Hidden
      [@@deriving sexp]
    end

    val body : t -> string
    val author : t -> Username.t
    val subreddit : t -> Subreddit_name.t
    val depth : t -> int option
    val score : t -> Score.t
    val replies : t -> [ `Comment of t | `More_comments of More_comments.t ] list
  end

  and User : sig
    include S

    val name : t -> Username.t
    val creation_time : t -> Time_ns.t
    val link_karma : t -> int
    val comment_karma : t -> int
    val awarder_karma : t -> int
    val awardee_karma : t -> int
    val total_karma : t -> int
    val subreddit : t -> Subreddit.t
  end

  and Link : sig
    include S

    val title : t -> string
    val author : t -> Username.t
    val url : t -> Uri.t option
    val subreddit : t -> Subreddit_name.t
    val is_stickied : t -> bool
    val creation_time : t -> Time_ns.t
    val score : t -> int
  end

  and Message : sig
    include S

    val author : t -> Username.t
  end

  and Subreddit : sig
    include S

    val name : t -> Subreddit_name.t
    val title : t -> string
    val description : t -> string
    val subscribers : t -> int
    val active_users : t -> int
    val creation_time : t -> Time_ns.t
  end

  and Award : sig
    include S
  end

  and More_comments : sig
    include S

    module Details : sig
      module By_children : sig
        type t

        val children : t -> Comment.Id.t list
      end

      type t =
        | By_children of By_children.t
        | By_parent of Comment.Id.t
    end

    val count : t -> int
    val details : t -> Details.t
  end

  and Modmail_conversation : sig
    include S
  end

  module Fullname : sig
    type t =
      [ `Comment of Comment.Id.t
      | `User of User.Id.t
      | `Link of Link.Id.t
      | `Message of Message.Id.t
      | `Subreddit of Subreddit.Id.t
      | `Award of Award.Id.t
      | `More_comments of More_comments.Id.t
      | `Modmail_conversation of Modmail_conversation.Id.t
      ]
    [@@deriving sexp]

    include Identifiable.S with type t := t

    val to_string : [< t ] -> string
    val of_string : string -> [> t ]
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
    val fullname : [< t ] -> [> Fullname.t ]
  end
end
