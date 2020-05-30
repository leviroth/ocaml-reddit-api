open! Core
open! Async

val iter
  :  (module Hashable.S with type t = 'id)
  -> Connection.t
  -> get_listing:
       (Connection.t
        -> before:'id option
        -> limit:int
        -> ('thing list, Cohttp.Response.t * Cohttp_async.Body.t) result Deferred.t)
  -> get_before_parameter:('thing -> 'id)
  -> f:('thing -> unit Deferred.t)
  -> 'b Deferred.t
