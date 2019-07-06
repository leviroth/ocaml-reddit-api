open! Core
open Async

type 'a call =
  ?param_list_override:((string * string sexp_list) sexp_list
                        -> (string * string sexp_list) sexp_list)
  -> Connection.t
  -> 'a Deferred.t

(* Links and comments *)

module Comment_sort : sig
  type t =
    | Confidence
    | Top
    | New
    | Controversial
    | Old
    | Random
    | Q_and_a
    | Live
  [@@deriving sexp]
end

val add_comment
  :  ?return_rtjson:bool
  -> ?richtext_json:Yojson.Safe.t
  -> parent:Fullname.t
  -> text:string
  -> (Cohttp.Response.t * Cohttp_async.Body.t) call

val delete : fullname:Fullname.t -> (Cohttp.Response.t * Cohttp_async.Body.t) call

val edit
  :  ?return_rtjson:bool
  -> ?richtext_json:Yojson.Safe.t
  -> fullname:Fullname.t
  -> text:string
  -> (Cohttp.Response.t * Cohttp_async.Body.t) call

val follow
  :  submission:Fullname.t
  -> follow:bool
  -> (Cohttp.Response.t * Cohttp_async.Body.t) call

val hide : submissions:Fullname.t list -> (Cohttp.Response.t * Cohttp_async.Body.t) call

val unhide
  :  submissions:Fullname.t list
  -> (Cohttp.Response.t * Cohttp_async.Body.t) call

val lock : fullname:Fullname.t -> (Cohttp.Response.t * Cohttp_async.Body.t) call
val unlock : fullname:Fullname.t -> (Cohttp.Response.t * Cohttp_async.Body.t) call
val mark_nsfw : fullname:Fullname.t -> (Cohttp.Response.t * Cohttp_async.Body.t) call
val unmark_nsfw : fullname:Fullname.t -> (Cohttp.Response.t * Cohttp_async.Body.t) call

val more_children
  :  ?id:Fullname.More_children_id.t
  -> ?limit_children:bool
  -> submission:Fullname.t
  -> children:Fullname.Comment_id.t list
  -> sort:Comment_sort.t
  -> (Cohttp.Response.t * Cohttp_async.Body.t) call

module Report_target : sig
  type t =
    | Modmail_conversation of Fullname.Modmail_conversation_id.t
    | Fullname of Fullname.t
  [@@deriving sexp]
end

val report
  :  ?from_modmail:bool
  -> ?from_help_desk:bool
  -> ?additional_info:string
  -> ?custom_text:string
  -> ?other_reason:string
  -> ?rule_reason:string
  -> ?site_reason:string
  -> ?sr_name:string
  -> target:Report_target.t
  -> reason:string
  -> (Cohttp.Response.t * Cohttp_async.Body.t) call

val report_award : award_id:string -> (Cohttp.Response.t * Cohttp_async.Body.t) call

val save
  :  ?category:string
  -> fullname:Fullname.t
  -> (Cohttp.Response.t * Cohttp_async.Body.t) call

val unsave : fullname:Fullname.t -> (Cohttp.Response.t * Cohttp_async.Body.t) call
val saved_categories : (Cohttp.Response.t * Cohttp_async.Body.t) call

val send_replies
  :  fullname:Fullname.t
  -> enabled:string
  -> (Cohttp.Response.t * Cohttp_async.Body.t) call

val set_contest_mode
  :  fullname:Fullname.t
  -> enabled:string
  -> (Cohttp.Response.t * Cohttp_async.Body.t) call

module Sticky_state : sig
  type t =
    | Sticky of { slot : int }
    | Unsticky
  [@@deriving sexp]
end

val set_subreddit_sticky
  :  ?to_profile:bool
  -> fullname:Fullname.t
  -> sticky_state:Sticky_state.t
  -> (Cohttp.Response.t * Cohttp_async.Body.t) call

val set_suggested_sort
  :  fullname:Fullname.t
  -> sort:Comment_sort.t option
  -> (Cohttp.Response.t * Cohttp_async.Body.t) call

val spoiler : fullname:Fullname.t -> (Cohttp.Response.t * Cohttp_async.Body.t) call
val unspoiler : fullname:Fullname.t -> (Cohttp.Response.t * Cohttp_async.Body.t) call

val store_visits
  :  submissions:Fullname.t list
  -> (Cohttp.Response.t * Cohttp_async.Body.t) call

module Submission_kind : sig
  module Self_post_body : sig
    type t =
      | Markdown of string
      | Richtext_json of Json_derivers.Yojson.t
    [@@deriving sexp]
  end

  type t =
    | Link of { url : string }
    | Self of Self_post_body.t
    | Image
    | Video
    | Videogif
  [@@deriving sexp]
end

val submit
  :  ?ad:bool
  -> ?nsfw:bool
  -> ?resubmit:bool
  -> ?sendreplies:bool
  -> ?spoiler:bool
  -> ?flair_id:string
  -> ?flair_text:string
  -> ?collection_id:string
  -> ?event_start:Time.t
  -> ?event_end:Time.t
  -> ?event_tz:string
  -> subreddit:Subreddit_name.t
  -> title:string
  -> kind:Submission_kind.t
  -> (Cohttp.Response.t * Cohttp_async.Body.t) call

module Vote_direction : sig
  type t =
    | Up
    | Neutral
    | Down
  [@@deriving sexp]
end

val vote
  :  ?rank:int
  -> direction:Vote_direction.t
  -> fullname:Fullname.t
  -> (Cohttp.Response.t * Cohttp_async.Body.t) call

module Info_query : sig
  type t =
    | Id of Fullname.t list
    | Url of Uri.t
  [@@deriving sexp]
end

val info
  :  ?subreddit:Subreddit_name.t
  -> Info_query.t
  -> (Cohttp.Response.t * Cohttp_async.Body.t) call

(** Listings *)

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
  -> submission:Fullname.Submission_id.t
  -> (Cohttp.Response.t * Cohttp_async.Body.t) call

(** Users *)

module Relationship : sig
  module Duration : sig
    type t =
      | Permanent
      | Days of int
    [@@deriving sexp]
  end

  type t =
    | Friend
    | Moderator
    | Moderator_invite
    | Contributor
    | Banned
    | Muted
    | Wiki_banned
    | Wiki_contributor
  [@@deriving sexp]
end

val add_relationship
  :  relationship:Relationship.t
  -> username:Username.t
  -> subreddit:Subreddit_name.t
  -> duration:Relationship.Duration.t
  -> ?note:string
  -> ?ban_reason:string
  -> ?ban_message:string
  -> ?ban_context:Fullname.t
  -> (Cohttp.Response.t * Cohttp_async.Body.t) call

val remove_relationship
  :  relationship:Relationship.t
  -> username:Username.t
  -> subreddit:Subreddit_name.t
  -> (Cohttp.Response.t * Cohttp_async.Body.t) call
