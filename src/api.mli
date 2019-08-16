open! Core
open Async

type 'a call =
  ?param_list_override:((string * string list) list -> (string * string list) list)
  -> Connection.t
  -> 'a Deferred.t

module Listing_params : sig
  module Pagination : sig
    module Before_or_after : sig
      type t =
        | Before
        | After
      [@@deriving sexp]
    end

    type t =
      { before_or_after : Before_or_after.t
      ; index : Fullname.t
      ; count : int
      }
    [@@deriving sexp]
  end

  type t =
    { pagination : Pagination.t option
    ; limit : int option
    ; show_all : bool
    }
  [@@deriving sexp]
end

(** Account *)

val me : (Cohttp.Response.t * Cohttp_async.Body.t) call
val karma : (Cohttp.Response.t * Cohttp_async.Body.t) call
val trophies : (Cohttp.Response.t * Cohttp_async.Body.t) call

val friends
  :  ?listing_params:Listing_params.t
  -> ?subreddit_detail:bool
  -> ?include_categories:bool
  -> (Cohttp.Response.t * Cohttp_async.Body.t) call

val blocked
  :  ?listing_params:Listing_params.t
  -> ?subreddit_detail:bool
  -> ?include_categories:bool
  -> (Cohttp.Response.t * Cohttp_async.Body.t) call

val messaging
  :  ?listing_params:Listing_params.t
  -> ?subreddit_detail:bool
  -> ?include_categories:bool
  -> (Cohttp.Response.t * Cohttp_async.Body.t) call

val trusted
  :  ?listing_params:Listing_params.t
  -> ?subreddit_detail:bool
  -> ?include_categories:bool
  -> (Cohttp.Response.t * Cohttp_async.Body.t) call

(** Captcha *)

val needs_captcha : (Cohttp.Response.t * Cohttp_async.Body.t) call

(** Links and comments *)

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
  :  ?id:Id36.More_children.t
  -> ?limit_children:bool
  -> submission:Fullname.t
  -> children:Id36.Comment.t list
  -> sort:Comment_sort.t
  -> (Cohttp.Response.t * Cohttp_async.Body.t) call

module Report_target : sig
  type t =
    | Modmail_conversation of Id36.Modmail_conversation.t
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
  -> enabled:bool
  -> (Cohttp.Response.t * Cohttp_async.Body.t) call

val set_contest_mode
  :  fullname:Fullname.t
  -> enabled:bool
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

val best
  :  ?include_categories:bool
  -> ?listing_params:Listing_params.t
  -> ?subreddit_detail:bool
  -> (Cohttp.Response.t * Cohttp_async.Body.t) call

val by_id : fullnames:Fullname.t list -> (Cohttp.Response.t * Cohttp_async.Body.t) call

module Duplicate_sort : sig
  type t =
    | Number_of_comments
    | New
end

val comments
  :  ?subreddit:Subreddit_name.t
  -> ?comment:Id36.Comment.t
  -> ?context:int
  -> ?depth:int
  -> ?limit:int
  -> ?showedits:bool
  -> ?showmore:bool
  -> ?sort:Comment_sort.t
  -> ?subreddit_detail:bool
  -> ?threaded:bool
  -> ?truncate:int
  -> submission:Id36.Submission.t
  -> (Cohttp.Response.t * Cohttp_async.Body.t) call

val duplicates
  :  ?crossposts_only:bool
  -> ?listing_params:Listing_params.t
  -> ?subreddit_detail:bool
  -> ?sort:Duplicate_sort.t
  -> submission_id:Id36.Submission.t
  -> (Cohttp.Response.t * Cohttp_async.Body.t) call

module Historical_span : sig
  type t =
    | Hour
    | Day
    | Week
    | Month
    | Year
    | All
  [@@deriving sexp]
end

val hot
  :  ?location:string
  -> ?include_categories:bool
  -> ?listing_params:Listing_params.t
  -> ?subreddit_detail:bool
  -> ?subreddit:Subreddit_name.t
  -> (Cohttp.Response.t * Cohttp_async.Body.t) call

val new_
  :  ?include_categories:bool
  -> ?listing_params:Listing_params.t
  -> ?subreddit_detail:bool
  -> ?subreddit:Subreddit_name.t
  -> (Cohttp.Response.t * Cohttp_async.Body.t) call

val rising
  :  ?include_categories:bool
  -> ?listing_params:Listing_params.t
  -> ?subreddit_detail:bool
  -> ?subreddit:Subreddit_name.t
  -> (Cohttp.Response.t * Cohttp_async.Body.t) call

val top
  :  ?since:Historical_span.t
  -> ?include_categories:bool
  -> ?listing_params:Listing_params.t
  -> ?subreddit_detail:bool
  -> ?subreddit:Subreddit_name.t
  -> (Cohttp.Response.t * Cohttp_async.Body.t) call

val controversial
  :  ?since:Historical_span.t
  -> ?include_categories:bool
  -> ?listing_params:Listing_params.t
  -> ?subreddit_detail:bool
  -> ?subreddit:Subreddit_name.t
  -> (Cohttp.Response.t * Cohttp_async.Body.t) call

val random
  :  ?subreddit:Subreddit_name.t
  -> (Cohttp.Response.t * Cohttp_async.Body.t) call

val trending_subreddits : (Cohttp.Response.t * Cohttp_async.Body.t) call

(** Private messages *)

val block : fullname:Fullname.t -> (Cohttp.Response.t * Cohttp_async.Body.t) call

val collapse_message
  :  fullnames:Fullname.t sexp_list
  -> (Cohttp.Response.t * Cohttp_async.Body.t) call

val uncollapse_message
  :  fullnames:Fullname.t sexp_list
  -> (Cohttp.Response.t * Cohttp_async.Body.t) call

val compose
  :  ?g_recaptcha_response:string
  -> ?from_subreddit:Subreddit_name.t
  -> to_:Username.t
  -> subject:string
  -> text:string
  -> (Cohttp.Response.t * Cohttp_async.Body.t) call

val delete_message
  :  fullname:Fullname.t
  -> (Cohttp.Response.t * Cohttp_async.Body.t) call

val read_message
  :  fullnames:Fullname.t sexp_list
  -> (Cohttp.Response.t * Cohttp_async.Body.t) call

val unread_message
  :  fullnames:Fullname.t sexp_list
  -> (Cohttp.Response.t * Cohttp_async.Body.t) call

val unblock_subreddit
  :  fullnames:Fullname.t sexp_list
  -> (Cohttp.Response.t * Cohttp_async.Body.t) call

val inbox
  :  ?include_categories:sexp_bool
  -> ?listing_params:Listing_params.t
  -> ?mid:string
  -> ?subreddit_detail:sexp_bool
  -> mark_read:sexp_bool sexp_list
  -> (Cohttp.Response.t * Cohttp_async.Body.t) call

val unread
  :  ?include_categories:sexp_bool
  -> ?listing_params:Listing_params.t
  -> ?mid:string
  -> ?subreddit_detail:sexp_bool
  -> mark_read:sexp_bool sexp_list
  -> (Cohttp.Response.t * Cohttp_async.Body.t) call

val sent
  :  ?include_categories:sexp_bool
  -> ?listing_params:Listing_params.t
  -> ?mid:string
  -> ?subreddit_detail:sexp_bool
  -> mark_read:sexp_bool sexp_list
  -> (Cohttp.Response.t * Cohttp_async.Body.t) call

(** Moderation *)

module Mod_filter : sig
  type t =
    | Moderators of Username.t list
    | Admin
end

val log
  :  ?listing_params:Listing_params.t
  -> ?mod_filter:Mod_filter.t
  -> ?subreddit_detail:bool
  -> ?subreddit:Subreddit_name.t
  -> ?type_:string
  -> (Cohttp.Response.t * Cohttp_async.Body.t) call

module Links_or_comments : sig
  type t =
    | Links
    | Comments
end

val reports
  :  ?listing_params:Listing_params.t
  -> ?location:string
  -> ?only:Links_or_comments.t
  -> ?subreddit:Subreddit_name.t
  -> ?subreddit_detail:bool
  -> (Cohttp.Response.t * Cohttp_async.Body.t) call

val spam
  :  ?listing_params:Listing_params.t
  -> ?location:string
  -> ?only:Links_or_comments.t
  -> ?subreddit:Subreddit_name.t
  -> ?subreddit_detail:bool
  -> (Cohttp.Response.t * Cohttp_async.Body.t) call

val modqueue
  :  ?listing_params:Listing_params.t
  -> ?location:string
  -> ?only:Links_or_comments.t
  -> ?subreddit:Subreddit_name.t
  -> ?subreddit_detail:bool
  -> (Cohttp.Response.t * Cohttp_async.Body.t) call

val unmoderated
  :  ?listing_params:Listing_params.t
  -> ?location:string
  -> ?only:Links_or_comments.t
  -> ?subreddit:Subreddit_name.t
  -> ?subreddit_detail:bool
  -> (Cohttp.Response.t * Cohttp_async.Body.t) call

val edited
  :  ?listing_params:Listing_params.t
  -> ?location:string
  -> ?only:Links_or_comments.t
  -> ?subreddit:Subreddit_name.t
  -> ?subreddit_detail:bool
  -> (Cohttp.Response.t * Cohttp_async.Body.t) call

val accept_moderator_invite
  :  subreddit:Subreddit_name.t
  -> (Cohttp.Response.t * Cohttp_async.Body.t) call

val approve : fullname:Fullname.t -> (Cohttp.Response.t * Cohttp_async.Body.t) call
val remove : fullname:Fullname.t -> (Cohttp.Response.t * Cohttp_async.Body.t) call

module How_to_distinguish : sig
  type t =
    | Mod
    | Admin
    | Special
    | Undistinguish
end

val distinguish
  :  ?sticky:bool
  -> fullname:Fullname.t
  -> how:How_to_distinguish.t
  -> (Cohttp.Response.t * Cohttp_async.Body.t) call

val ignore_reports
  :  fullname:Fullname.t
  -> (Cohttp.Response.t * Cohttp_async.Body.t) call

val unignore_reports
  :  fullname:Fullname.t
  -> (Cohttp.Response.t * Cohttp_async.Body.t) call

val leavecontributor
  :  fullname:Fullname.t
  -> (Cohttp.Response.t * Cohttp_async.Body.t) call

val leavemoderator
  :  fullname:Fullname.t
  -> (Cohttp.Response.t * Cohttp_async.Body.t) call

val mute_message_author
  :  fullname:Fullname.t
  -> (Cohttp.Response.t * Cohttp_async.Body.t) call

val unmute_message_author
  :  fullname:Fullname.t
  -> (Cohttp.Response.t * Cohttp_async.Body.t) call

val stylesheet
  :  subreddit:Subreddit_name.t
  -> (Cohttp.Response.t * Cohttp_async.Body.t) call

(** Search *)

module Search_sort : sig
  type t =
    | Relevance
    | Hot
    | Top
    | New
    | Comments
end

module Search_type : sig
  type t =
    | Subreddit
    | Submission
    | User
  [@@deriving sexp]

  include Comparable.S with type t := t
end

val search
  :  ?category:string
  -> ?include_facets:bool
  -> ?listing_params:Listing_params.t
  -> ?restrict_to_subreddit:Subreddit_name.t
  -> ?since:Historical_span.t
  -> ?sort:Search_sort.t
  -> ?subreddit_detail:bool
  -> ?types:Search_type.Set.t
  -> query:string
  -> (Cohttp.Response.t * Cohttp_async.Body.t) call

(** Subreddits *)

val banned
  :  ?include_categories:bool
  -> ?listing_params:Listing_params.t
  -> ?subreddit_detail:bool
  -> ?user:Username.t
  -> subreddit:Subreddit_name.t
  -> (Cohttp.Response.t * Cohttp_async.Body.t) call

val muted
  :  ?include_categories:bool
  -> ?listing_params:Listing_params.t
  -> ?subreddit_detail:bool
  -> ?user:Username.t
  -> subreddit:Subreddit_name.t
  -> (Cohttp.Response.t * Cohttp_async.Body.t) call

val wiki_banned
  :  ?include_categories:bool
  -> ?listing_params:Listing_params.t
  -> ?subreddit_detail:bool
  -> ?user:Username.t
  -> subreddit:Subreddit_name.t
  -> (Cohttp.Response.t * Cohttp_async.Body.t) call

val contributors
  :  ?include_categories:bool
  -> ?listing_params:Listing_params.t
  -> ?subreddit_detail:bool
  -> ?user:Username.t
  -> subreddit:Subreddit_name.t
  -> (Cohttp.Response.t * Cohttp_async.Body.t) call

val wiki_contributors
  :  ?include_categories:bool
  -> ?listing_params:Listing_params.t
  -> ?subreddit_detail:bool
  -> ?user:Username.t
  -> subreddit:Subreddit_name.t
  -> (Cohttp.Response.t * Cohttp_async.Body.t) call

val moderators
  :  ?include_categories:bool
  -> ?listing_params:Listing_params.t
  -> ?subreddit_detail:bool
  -> ?user:Username.t
  -> subreddit:Subreddit_name.t
  -> (Cohttp.Response.t * Cohttp_async.Body.t) call

val delete_subreddit_banner
  :  subreddit:Subreddit_name.t
  -> (Cohttp.Response.t * Cohttp_async.Body.t) call

val delete_subreddit_header
  :  subreddit:Subreddit_name.t
  -> (Cohttp.Response.t * Cohttp_async.Body.t) call

val delete_subreddit_icon
  :  subreddit:Subreddit_name.t
  -> (Cohttp.Response.t * Cohttp_async.Body.t) call

val delete_subreddit_image
  :  image_name:string
  -> subreddit:Subreddit_name.t
  -> (Cohttp.Response.t * Cohttp_async.Body.t) call

val recommended
  :  ?over_18:bool
  -> subreddits:Subreddit_name.t list
  -> (Cohttp.Response.t * Cohttp_async.Body.t) call

val search_subreddit_names
  :  ?exact:bool
  -> ?include_over_18:bool
  -> ?include_unadvertisable:bool
  -> query:string
  -> (Cohttp.Response.t * Cohttp_async.Body.t) call

module Link_type : sig
  type t =
    | Any
    | Link
    | Self
end

module Spam_level : sig
  type t =
    | Low
    | High
    | All
end

module Subreddit_type : sig
  type t =
    | Gold_restricted
    | Archived
    | Restricted
    | Employees_only
    | Gold_only
    | Private
    | User
    | Public
end

module Wiki_mode : sig
  type t =
    | Disabled
    | Mod_only
    | Anyone
end

val create_or_edit_subreddit
  :  ?comment_score_hide_mins:int
  -> ?wiki_edit_age:int
  -> ?wiki_edit_karma:int
  -> all_original_content:bool
  -> allow_discovery:bool
  -> allow_images:bool
  -> allow_post_crossposts:bool
  -> allow_top:bool
  -> allow_videos:bool
  -> api_type:(string * string list) list
  -> collapse_deleted_comments:bool
  -> crowd_control_mode:bool
  -> description:string
  -> disable_contributor_requests:bool
  -> exclude_banned_modqueue:bool
  -> free_form_reports:bool
  -> g_recaptcha_response:string option
  -> header_title:string
  -> hide_ads:bool
  -> key_color:string
  -> lang:string
  -> link_type:Link_type.t
  -> name:string
  -> original_content_tag_enabled:bool
  -> over_18:bool
  -> public_description:string
  -> restrict_commenting:bool
  -> restrict_posting:bool
  -> show_media:bool
  -> show_media_preview:bool
  -> spam_comments:Spam_level.t
  -> spam_links:Spam_level.t
  -> spam_selfposts:Spam_level.t
  -> spoilers_enabled:bool
  -> subreddit:Subreddit_name.t
  -> submit_link_label:string
  -> submit_text:string
  -> submit_text_label:string
  -> suggested_comment_sort:Comment_sort.t
  -> title:string
  -> type_:Subreddit_type.t
  -> wiki_mode:Wiki_mode.t
  -> (Cohttp.Response.t * Cohttp_async.Body.t) call

val submit_text
  :  subreddit:Subreddit_name.t
  -> (Cohttp.Response.t * Cohttp_async.Body.t) call

val subreddit_autocomplete
  :  ?include_over_18:bool
  -> ?include_profiles:bool
  -> query:string
  -> (Cohttp.Response.t * Cohttp_async.Body.t) call

val subreddit_autocomplete_v2
  :  ?limit:int
  -> ?include_categories:bool
  -> ?include_over_18:bool
  -> ?include_profiles:bool
  -> query:string
  -> (Cohttp.Response.t * Cohttp_async.Body.t) call

module Stylesheet_operation : sig
  type t =
    | Save
    | Preview
end

val subreddit_stylesheet
  :  ?reason:string
  -> operation:Stylesheet_operation.t
  -> stylesheet_contents:string
  -> subreddit:Subreddit_name.t
  -> (Cohttp.Response.t * Cohttp_async.Body.t) call

module Subscription_action : sig
  type t =
    | Subscribe
    | Unsubscribe
end

val subscribe
  :  ?skip_initial_defaults:bool
  -> action:Subscription_action.t
  -> (Cohttp.Response.t * Cohttp_async.Body.t) call

module Image_type : sig
  type t =
    | Png
    | Jpg
end

module Upload_type : sig
  type t =
    | Image
    | Header
    | Icon
    | Banner
end

val upload_sr_img
  :  ?form_id:string
  -> file:string
  -> header:bool
  -> image_type:Image_type.t
  -> name:string
  -> subreddit:Subreddit_name.t
  -> upload_type:Upload_type.t
  -> (Cohttp.Response.t * Cohttp_async.Body.t) call

module Subreddit_search_sort : sig
  type t =
    | Relevance
    | Activity
end

val search_profiles
  :  ?listing_params:Listing_params.t
  -> ?subreddit_detail:bool
  -> ?sort:Subreddit_search_sort.t
  -> query:string
  -> (Cohttp.Response.t * Cohttp_async.Body.t) call

val about : subreddit:Subreddit_name.t -> (Cohttp.Response.t * Cohttp_async.Body.t) call

val subreddit_settings
  :  ?created:bool
  -> ?location:string
  -> subreddit:Subreddit_name.t
  -> (Cohttp.Response.t * Cohttp_async.Body.t) call

val subreddit_rules
  :  subreddit:Subreddit_name.t
  -> (Cohttp.Response.t * Cohttp_async.Body.t) call

val subreddit_traffic
  :  subreddit:Subreddit_name.t
  -> (Cohttp.Response.t * Cohttp_async.Body.t) call

val subreddit_sidebar
  :  subreddit:Subreddit_name.t
  -> (Cohttp.Response.t * Cohttp_async.Body.t) call

val sticky
  :  ?number:int
  -> subreddit:Subreddit_name.t
  -> (Cohttp.Response.t * Cohttp_async.Body.t) call

module Subreddit_relationship : sig
  type t =
    | Subscriber
    | Contributor
    | Moderator
    | Stream_subscriber
end

val get_subreddits
  :  ?include_categories:bool
  -> ?listing_params:Listing_params.t
  -> ?subreddit_detail:string
  -> relationship:Subreddit_relationship.t
  -> (Cohttp.Response.t * Cohttp_async.Body.t) call

val search_subreddits
  :  ?listing_params:Listing_params.t
  -> ?show_users:bool
  -> ?sort:Subreddit_search_sort.t
  -> ?subreddit_detail:bool
  -> query:string
  -> (Cohttp.Response.t * Cohttp_async.Body.t) call

module Subreddit_listing_sort : sig
  type t =
    | Popular
    | New
    | Gold
    | Default
end

val list_subreddits
  :  ?listing_params:Listing_params.t
  -> ?subreddit_detail:bool
  -> ?include_categories:bool
  -> ?show_users:bool
  -> sort:Subreddit_listing_sort.t
  -> (Cohttp.Response.t * Cohttp_async.Body.t) call

module User_subreddit_sort : sig
  type t =
    | Popular
    | New
end

val list_user_subreddits
  :  ?listing_params:Listing_params.t
  -> ?subreddit_detail:bool
  -> ?include_categories:bool
  -> sort:User_subreddit_sort.t
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

(** Wiki *)

module Wiki_page : sig
  type t =
    { subreddit : Subreddit_name.t
    ; page : string
    }
  [@@deriving sexp]
end

module Add_or_remove : sig
  type t =
    | Add
    | Remove
end

val add_or_remove_wiki_editor
  :  add_or_remove:Add_or_remove.t
  -> page:Wiki_page.t
  -> user:Username.t
  -> (Cohttp.Response.t * Cohttp_async.Body.t) call

val edit_wiki_page
  :  ?previous:string
  -> ?reason:string
  -> content:string
  -> page:Wiki_page.t
  -> (Cohttp.Response.t * Cohttp_async.Body.t) call

val toggle_wiki_revision_visibility
  :  page:Wiki_page.t
  -> revision:string
  -> (Cohttp.Response.t * Cohttp_async.Body.t) call

val revert_wiki_page
  :  page:Wiki_page.t
  -> revision:string
  -> (Cohttp.Response.t * Cohttp_async.Body.t) call

val wiki_discussions
  :  ?listing_params:Listing_params.t
  -> ?subreddit_detail:string
  -> page:Wiki_page.t
  -> (Cohttp.Response.t * Cohttp_async.Body.t) call

val wiki_pages
  :  subreddit:Subreddit_name.t
  -> (Cohttp.Response.t * Cohttp_async.Body.t) call

val subreddit_wiki_revisions
  :  ?listing_params:Listing_params.t
  -> ?subreddit_detail:string
  -> subreddit:Subreddit_name.t
  -> (Cohttp.Response.t * Cohttp_async.Body.t) call

val wiki_page_revisions
  :  ?listing_params:Listing_params.t
  -> ?subreddit_detail:string
  -> page:Wiki_page.t
  -> (Cohttp.Response.t * Cohttp_async.Body.t) call

val wiki_permissions : page:Wiki_page.t -> (Cohttp.Response.t * Cohttp_async.Body.t) call

val set_wiki_permissions
  :  listed:bool
  -> page:Wiki_page.t
  -> permission_level:int
  -> (Cohttp.Response.t * Cohttp_async.Body.t) call

val wiki_page
  :  ?compare_revisions:string option * string option
  -> page:Wiki_page.t
  -> (Cohttp.Response.t * Cohttp_async.Body.t) call
