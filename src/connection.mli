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

module Sequencer : sig
  type t = More_children [@@deriving sexp]
end

type t [@@deriving sexp_of]

val create : Credentials.t -> user_agent:string -> t

val post_form
  :  ?sequence:Sequencer.t
  -> t
  -> Uri.t
  -> params:(string * string list) list
  -> (Cohttp.Response.t * Cohttp_async.Body.t, Exn.t) Deferred.Result.t

val get
  :  ?sequence:Sequencer.t
  -> t
  -> Uri.t
  -> (Cohttp.Response.t * Cohttp_async.Body.t, Exn.t) Deferred.Result.t

module Remote : sig
  (** Any connection can be turned into an RPC server, acting as a shared
   *  connection for multiple client [Connection.t]s. Rate limiting is managed
   *  on the server side. *)

  val serve
    :  t
    -> where_to_listen:(([< Socket.Address.t ] as 'a), 'b) Tcp.Where_to_listen.t
    -> ('a, 'b) Tcp.Server.t Deferred.t

  val connect_exn : [< Socket.Address.t ] Tcp.Where_to_connect.t -> t Deferred.t
end

module For_testing : sig
  val with_cassette
    :  Filename.t
    -> credentials:Credentials.t
    -> f:(t -> 'a Deferred.t)
    -> 'a Deferred.t
end
