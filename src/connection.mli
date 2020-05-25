open! Core
open Async

module Config : sig
  type t =
    { client_id : string
    ; client_secret : string
    ; password : string
    ; username : string
    }
  [@@deriving sexp]
end

type t [@@deriving sexp_of]

val create : Config.t -> user_agent:string -> t
val more_children_sequencer : t -> unit Sequencer.t

val post_form
  :  t
  -> Uri.t
  -> params:(string * string list) list
  -> (Cohttp.Response.t * Cohttp_async.Body.t) Deferred.t

val get : t -> Uri.t -> (Cohttp.Response.t * Cohttp_async.Body.t) Deferred.t

module For_testing : sig
  module type Cohttp_client_wrapper = sig
    val get
      :  Uri.t
      -> headers:Cohttp.Header.t
      -> (Cohttp.Response.t * Cohttp_async.Body.t) Deferred.t

    val post_form
      :  Uri.t
      -> headers:Cohttp.Header.t
      -> params:(string * string list) list
      -> (Cohttp.Response.t * Cohttp_async.Body.t) Deferred.t
  end

  val create
    :  (module Cohttp_client_wrapper)
    -> Config.t
    -> time_source:Time_source.t
    -> t
end
