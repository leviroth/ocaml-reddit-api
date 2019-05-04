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

module Auth : sig
  type t [@@deriving sexp]

  val create : Config.t -> unit -> t
end

type t [@@deriving sexp]

val create : Config.t -> t

val with_t
  :  t
  -> f:(Cohttp.Header.t -> (Cohttp.Response.t * Cohttp_async.Body.t) Deferred.t)
  -> headers:Cohttp.Header.t
  -> (Cohttp.Response.t * Cohttp_async.Body.t) Deferred.t
