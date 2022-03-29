open! Core

type t =
  { moderator : Username.t option
  ; report : string
  }

val of_json : Jsonaf.t -> t
