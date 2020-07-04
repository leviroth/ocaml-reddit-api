open! Core
open Async

module Credentials : sig
  type t =
    { client_id : string
    ; client_secret : string
    ; password : string
    ; username : string
    }
  [@@deriving sexp]
end

type t [@@deriving sexp_of]

val create : Credentials.t -> user_agent:string -> t
val more_children_sequencer : t -> unit Sequencer.t

val post_form
  :  t
  -> Uri.t
  -> params:(string * string list) list
  -> (Cohttp.Response.t * Cohttp_async.Body.t, Exn.t) Deferred.Result.t

val get : t -> Uri.t -> (Cohttp.Response.t * Cohttp_async.Body.t, Exn.t) Deferred.Result.t

module For_testing : sig
  val with_cassette
    :  Filename.t
    -> credentials:Credentials.t
    -> f:(t -> 'a Deferred.t)
    -> 'a Deferred.t
end
