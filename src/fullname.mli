open! Core

(* TODO: Submission_id, etc? *)
module type Thing_id = sig
  type t [@@deriving sexp]

  include Stringable.S with type t := t

  val of_int : int -> t
  val to_int : t -> int
end

module Comment_id : Thing_id
module User_id : Thing_id
module Submission_id : Thing_id
module More_children_id : Thing_id
module Modmail_conversation_id : Thing_id

type t =
  | Comment of Comment_id.t
  | User of User_id.t
  | Submission of Submission_id.t
[@@deriving sexp]

include Stringable.S with type t := t
