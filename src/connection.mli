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

val create : Config.t -> t

val with_t
  :  t
  -> f:(Cohttp.Header.t -> (Cohttp.Response.t * Cohttp_async.Body.t) Deferred.t)
  -> headers:Cohttp.Header.t
  -> (Cohttp.Response.t * Cohttp_async.Body.t) Deferred.t

val with_retry
  :  t
  -> f:(Cohttp.Header.t -> (Cohttp.Response.t * Cohttp_async.Body.t) Deferred.t)
  -> headers:Cohttp.Header.t
  -> (Cohttp.Response.t * Cohttp_async.Body.t) Deferred.t

val post_form
  :  t
  -> Uri.t
  -> params:(string * string list) list
  -> (Cohttp.Response.t * Cohttp_async.Body.t) Deferred.t

val get : t -> Uri.t -> (Cohttp.Response.t * Cohttp_async.Body.t) Deferred.t
