open! Core

type t =
  { moderator : Username.t option
  ; report : string
  }

val t_of_jsonaf : Jsonaf.t -> t
