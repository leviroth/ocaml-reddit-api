open! Core
open! Async

module Comment_sort : sig
  type t =
    | Confidence
    | Top
    | New
    | Controversial
    | Old
    | Random
    | Qa
    | Live
  [@@deriving sexp]
end

module Info_query : sig
  type t =
    | Id of Fullname.t list
    | Url of Uri.t
end

val info
  :  ?subreddit:Subreddit_name.t
  -> Connection.t
  -> Info_query.t
  -> Thing.t list Deferred.t

val comments
  :  ?subreddit:Subreddit_name.t
  -> ?comment:Fullname.Comment_id.t
  -> ?context:int
  -> ?depth:int
  -> ?limit:int
  -> ?showedits:bool
  -> ?showmore:bool
  -> ?sort:Comment_sort.t
  -> ?sr_detail:bool
  -> ?threaded:bool
  -> ?truncate:int
  -> Connection.t
  -> submission:Fullname.Submission_id.t
  -> Thing.t list Deferred.t
