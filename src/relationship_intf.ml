open! Core

module type Id = sig
  type t [@@deriving sexp]

  include Stringable with type t := t
end

module type S = sig
  module Id : Id

  type t [@@deriving sexp]

  include Jsonable.S with type t := t

  val relationship_id : t -> Id.t
  val username : t -> Username.t
  val user_id : t -> Thing.User.Id.t
  val date : t -> Time_ns.t
end

module type Relationship = sig
  module Mute : sig
    module Id : sig
      include Id

      val to_uuid : t -> Uuid.t
      val of_uuid : Uuid.t -> t
    end

    include S with module Id := Id
  end

  module Ban : sig
    include S

    val note : t -> string
    val days_left : t -> int option
  end
end
