open! Core

module type S = Jsonaf.Jsonafable.S

module type S_with_fields = sig
  include S

  val get_field : t -> string -> Jsonaf.t option
  val get_field_exn : t -> string -> Jsonaf.t
  val field_map : t -> Jsonaf.t Map.M(String).t
end

module type S_with_kind = sig
  include S

  val kind : string
end

module type Kinded_param = sig
  type t

  val of_data_field : Jsonaf.t -> t
  val to_data_field : t -> Jsonaf.t
  val kind : string
end

module type Json_object = sig
  module type S = S
  module type S_with_fields = S_with_fields
  module type S_with_kind = S_with_kind

  module Utils : sig
    include S_with_fields with type t = Jsonaf.t Map.M(String).t
    include Sexpable.S with type t := t
    include module type of Of_json with type 'a t := 'a Of_json.t

    val optional_field : string -> (Jsonaf.t -> 'a) -> t -> 'a option
    val required_field : string -> (Jsonaf.t -> 'a) -> t -> 'a
    val username : Jsonaf.t -> Username.t
    val subreddit_name : Jsonaf.t -> Subreddit_name.t
    val time_sec_since_epoch : Jsonaf.t -> Time_ns.t
    val uri : Jsonaf.t -> Uri.t
  end

  module Make_kinded (Param : Kinded_param) : S_with_kind with type t := Param.t

  module Make_kinded_simple (Param : sig
    val kind : string
  end) : S_with_kind with type t := Utils.t
end
