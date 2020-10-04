open! Core
open! Async

val iter_comments
  :  Connection.t
  -> retry_manager:Retry_manager.t
  -> comment_response:Comment_response.t
  -> f:(Thing.Comment.t -> unit Deferred.t)
  -> unit Deferred.t
