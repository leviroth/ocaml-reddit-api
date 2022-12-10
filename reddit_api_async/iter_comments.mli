open! Core
open! Async
open Reddit_api_kernel

val iter_comments
  :  Retry_manager.t
  -> log:Log.t
  -> comment_response:Comment_response.t
  -> Thing.Comment.t Pipe.Reader.t
