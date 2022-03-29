open! Core

module type S = sig
  type t

  val of_json : Jsonaf.t -> t
  val to_json : t -> Jsonaf.t
end

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

    val optional_field : string -> (Jsonaf.t -> 'a) -> t -> 'a option
    val required_field : string -> (Jsonaf.t -> 'a) -> t -> 'a
    val ( >> ) : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c
    val or_null : (Jsonaf.t -> 'a) -> Jsonaf.t -> 'a option
    val int : Jsonaf.t -> int
    val float : Jsonaf.t -> float
    val bool : Jsonaf.t -> bool
    val string : Jsonaf.t -> string
    val username : Jsonaf.t -> Username.t
    val subreddit_name : Jsonaf.t -> Subreddit_name.t
    val time : Jsonaf.t -> Time_ns.t
    val uri : Jsonaf.t -> Uri.t
  end

  module Make_kinded (Param : Kinded_param) : S_with_kind with type t := Param.t

  module Make_kinded_simple (Param : sig
    val kind : string
  end) : S_with_kind with type t := Utils.t
end
