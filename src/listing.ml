open! Core

(* TODO: Maybe delete *)
type t =
  { after : Fullname.t option
  ; before : Fullname.t option
  ; limit : int
  ; children : Thing.t list
  ; dist : int
  ; modhash : string
  }
