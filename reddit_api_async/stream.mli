open! Core
open! Async
open Reddit_api_kernel

val stream
  :  (module Hashtbl.Key_plain with type t = 'id)
  -> Connection.t
  -> get_listing:(before:'id option -> limit:int -> 'thing list Endpoint.t)
  -> get_before_parameter:('thing -> 'id)
  -> ('thing, Endpoint.Error.t Connection.Error.t) Result.t Pipe.Reader.t
