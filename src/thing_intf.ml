open! Core

module type S = sig
  type t [@@deriving sexp]

  module Id36 : Id36.S

  val of_json : Json.t -> t
  val to_json : t -> Json.t
  val get_field : t -> string -> Json.t option
  val id36 : t -> Id36.t option
end

module Projectors = struct
  module Ident (M : S) = M
  module Id36 (M : S) = M.Id36
end

module type Thing = sig
  module type S = S

  module Comment : S
  module User : S
  module Link : S
  module Message : S
  module Subreddit : S
  module Award : S
  module More_comments : S
  module Modmail_conversation : S

  module type Per_kind = sig
    module F (M : S) : T

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

  val of_json : Json.t -> [> t ]
  val to_json : [< t ] -> Json.t
  val get_field : [< t ] -> string -> Json.t option
  val author : [< link | comment ] -> Username.t option
  val moderation_info : [< link | comment ] -> Moderation_info.t option

  module Fullname : sig
    include Per_kind with module F := Projectors.Id36

    val to_string : [< t ] -> string
    val of_string : string -> [> t ]
  end

  val fullname : [< t ] -> [> Fullname.t ] option
end
