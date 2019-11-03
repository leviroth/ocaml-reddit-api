open! Core

module type Id36 = sig
  type t [@@deriving sexp]

  include Stringable.S with type t := t

  val of_int : int -> t
  val to_int : t -> int
end

module type Common = sig
  type t [@@deriving sexp]

  module Id36 : Id36

  val of_json : Yojson.Safe.t -> t
  val to_json : t -> Yojson.Safe.t
  val get_field : t -> string -> Json_derivers.Yojson.t option
  val id36 : t -> Id36.t option
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

  module type Per_kind = sig
    module Comment : Common
    module User : Common
    module Link : Common
    module Message : Common
    module Subreddit : Common
    module Award : Common
  end

  module type By_kind = sig
    module Get_kind_module (M : Common) : Sexpable
    module Comment : Sexpable with type t = Get_kind_module(Comment).t
    module User : Sexpable with type t = Get_kind_module(User).t
    module Link : Sexpable with type t = Get_kind_module(Link).t
    module Message : Sexpable with type t = Get_kind_module(Message).t
    module Subreddit : Sexpable with type t = Get_kind_module(Subreddit).t
    module Award : Sexpable with type t = Get_kind_module(Award).t
  end

  (* module Data : sig
   *   module Get_kind_module = functor (M : Common) -> M
   *   include By_kind with module Get_kind_module := Get_kind_module
   *     end *)

  module Any : sig
    type t =
      | Comment of Comment.t
      | User of User.t
      | Link of Link.t
      | Message of Message.t
      | Subreddit of Subreddit.t
      | Award of Award.t
  end

  (* module Fullname : module type of By_kind ((functor (M : Common) -> M.Id36)) *)

  (*   type t =
   *     | Comment of Get_kind_module(Comment).t
   *     | User of Get_kind_module(User).t
   *     | Link of Get_kind_module(Link).t
   *     | Message of Get_kind_module(Message).t
   *     | Subreddit of Get_kind_module(Subreddit).t
   *     | Award of Get_kind_module(Award).t
   *   [@@deriving sexp]
   * 
   *   module Link_or_comment : sig
   *     type t =
   *       | Link of Get_kind_module(Link).t
   *       | Comment of Get_kind_module(Comment).t
   *     [@@deriving sexp]
   *   end
   * 
   *   module Link_or_comment_or_subreddit : sig
   *     type t =
   *       | Link of Get_kind_module(Link).t
   *       | Comment of Get_kind_module(Comment).t
   *       | Subreddit of Get_kind_module(Subreddit).t
   *     [@@deriving sexp]
   *   end
   * end *)

  val of_json : Yojson.Safe.t -> t
  val to_json : t -> Yojson.Safe.t
  val get_field : t -> string -> Json_derivers.Yojson.t option
end
