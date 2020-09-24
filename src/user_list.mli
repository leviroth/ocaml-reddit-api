open! Core

module Item : sig
  module Id : Identifiable

  type t [@@deriving sexp]

  include Jsonable.S with type t := t

  val username : t -> Username.t
  val user_id : t -> Thing.User.Id.t
  val relationship_id : t -> Id.t
  val since : t -> Time_ns.t
end

type t = Item.t list [@@deriving sexp]
