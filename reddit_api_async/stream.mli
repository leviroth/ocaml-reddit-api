open! Core
open! Async
open Reddit_api_kernel

val iter
  :  (module Hashtbl.Key_plain with type t = 'id)
  -> Connection.t
  -> get_listing:(before:'id option -> limit:int -> 'thing list Endpoint.t)
  -> get_before_parameter:('thing -> 'id)
  -> log:Log.t option
  -> f:('thing -> unit Deferred.t)
  -> _ Deferred.t

val fold
  :  (module Hashtbl.Key_plain with type t = 'id)
  -> Connection.t
  -> get_listing:(before:'id option -> limit:int -> 'thing list Endpoint.t)
  -> get_before_parameter:('thing -> 'id)
  -> init:'state
  -> f:('state -> 'thing -> 'state Deferred.t)
  -> on_error:('state -> Endpoint.Error.t Connection.Error.t -> 'state Deferred.t)
  -> _ Deferred.t

val fold_until_finished
  :  (module Hashtbl.Key_plain with type t = 'id)
  -> Connection.t
  -> get_listing:(before:'id option -> limit:int -> 'thing list Endpoint.t)
  -> get_before_parameter:('thing -> 'id)
  -> init:'state
  -> f:('state -> 'thing -> ('state, 'result) Continue_or_stop.t Deferred.t)
  -> on_error:
       ('state
        -> Endpoint.Error.t Connection.Error.t
        -> ('state, 'result) Continue_or_stop.t Deferred.t)
  -> 'result Deferred.t
