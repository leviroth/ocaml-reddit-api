open! Core

module Make (Hashable : Hashtbl.Key_plain) : sig
  type t

  val create : capacity:int -> t
  val add : t -> Hashable.t -> unit
  val mem : t -> Hashable.t -> bool
end
