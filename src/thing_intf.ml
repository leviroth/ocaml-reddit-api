open! Core

module type S = sig
  type t [@@deriving sexp]

  val of_json : Yojson.Safe.t -> t
end

module type Thing = sig
  include S with type t = Json_derivers.Yojson.t String.Map.t
end
