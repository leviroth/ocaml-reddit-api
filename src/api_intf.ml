open! Core
open Async
open Thing

module type Parameters = sig
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

  module Flair_target : sig
    type t =
      | Link of Link.Id.t
      | User of Username.t
  end

  module Color : sig
    type t

    val create : red:int -> green:int -> blue:int -> t
  end

  module Sticky_state : sig
    type t =
      | Sticky of { slot : int option }
      | Unsticky
    [@@deriving sexp]
  end

  module Link_kind : sig
    module Self_post_body : sig
      type t =
        | Markdown of string
        | Richtext_json of Json.t
      [@@deriving sexp]
    end

    type t =
      | Link of { url : string }
      | Self of Self_post_body.t
      | Crosspost of Link.Id.t
    [@@deriving sexp]
  end

  module Vote_direction : sig
    type t =
      | Up
      | Neutral
      | Down
    [@@deriving sexp]
  end

  module Info_query : sig
    type t =
      | Id of
          [ `Link of Link.Id.t | `Comment of Comment.Id.t | `Subreddit of Subreddit.Id.t ]
          list
      | Url of Uri.t
    [@@deriving sexp]
  end

  module Duplicate_sort : sig
    type t =
      | Number_of_comments
      | New
  end

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

  module Mod_filter : sig
    type t =
      | Moderators of Username.t list
      | Admin
  end

  module Links_or_comments : sig
    type t =
      | Links
      | Comments
  end

  module How_to_distinguish : sig
    type t =
      | Mod
      | Admin
      | Special
      | Undistinguish
  end

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
      | Link
      | User
    [@@deriving sexp, enumerate]

    include Comparable.S with type t := t
  end

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

  module Stylesheet_operation : sig
    type t =
      | Save
      | Preview
  end

  module Subscription_action : sig
    type t =
      | Subscribe
      | Unsubscribe
  end

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

  module Subreddit_search_sort : sig
    type t =
      | Relevance
      | Activity
  end

  module Subreddit_relationship : sig
    type t =
      | Subscriber
      | Contributor
      | Moderator
      | Stream_subscriber
  end

  module Subreddit_listing_sort : sig
    type t =
      | Popular
      | New
      | Gold
      | Default
  end

  module User_subreddit_sort : sig
    type t =
      | Popular
      | New
  end

  module Relationship_spec : sig
    module Duration : sig
      type t =
        | Permanent
        | Days of int
      [@@deriving sexp, compare, equal]
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

  module Add_or_remove : sig
    type t =
      | Add
      | Remove
  end
end

module type S = sig
  module Parameters : Parameters
  open Parameters

  type 'a with_listing_params :=
    ?pagination:Listing.Pagination.t -> ?count:int -> ?limit:int -> ?show_all:unit -> 'a

  type 'a response

  type 'a call :=
    ?param_list_override:((string * string list) list -> (string * string list) list)
    -> Connection.t
    -> 'a response Deferred.t

  (** Account *)

  val me : User.t call
  val karma : Karma_list.t call
  val trophies : Award.t list call
  val friends : User_list.t call with_listing_params
  val blocked : User_list.t call with_listing_params
  val messaging : User_list.t call with_listing_params
  val trusted : User_list.t call with_listing_params

  (** Flair *)

  val select_flair
    :  ?background_color:Color.t
    -> ?css_class:string
    -> ?flair_template_id:Uuid.t
    -> ?text:string
    -> ?text_color:Color.t
    -> subreddit:Subreddit_name.t
    -> target:Flair_target.t
    -> unit call

  (** Links and comments *)

  val add_comment
    :  ?return_rtjson:bool
    -> ?richtext_json:Json.t
    -> parent:
         [< `Link of Link.Id.t | `Comment of Comment.Id.t | `Message of Message.Id.t ]
    -> text:string
    -> Thing.Comment.t call

  val delete : id:[< `Link of Link.Id.t | `Comment of Comment.Id.t ] -> unit call

  val edit
    :  ?return_rtjson:bool
    -> ?richtext_json:Json.t
    -> id:[< `Link of Link.Id.t | `Comment of Comment.Id.t ]
    -> text:string
    -> [> `Link of Link.t | `Comment of Comment.t ] call

  val hide : links:Link.Id.t list -> unit call
  val unhide : links:Link.Id.t list -> unit call
  val lock : id:[< `Link of Link.Id.t | `Comment of Comment.Id.t ] -> unit call
  val unlock : id:[< `Link of Link.Id.t | `Comment of Comment.Id.t ] -> unit call
  val mark_nsfw : link:Link.Id.t -> unit call
  val unmark_nsfw : link:Link.Id.t -> unit call

  val more_children
    :  ?limit_children:bool
    -> link:Link.Id.t
    -> more_comments:More_comments.Details.By_children.t
    -> sort:Comment_sort.t
    -> [ `Comment of Comment.t | `More_comments of More_comments.t ] list call

  val report
    :  ?from_modmail:bool
    -> ?from_help_desk:bool
    -> ?additional_info:string
    -> ?custom_text:string
    -> ?other_reason:string
    -> ?rule_reason:string
    -> ?site_reason:string
    -> ?sr_name:string
    -> target:
         [< `Link of Link.Id.t
         | `Comment of Comment.Id.t
         | `Message of Message.Id.t
         | `Modmail_conversation of Modmail_conversation.Id.t
         ]
    -> reason:string
    -> unit call

  val report_award : award_id:string -> (Cohttp.Response.t * Cohttp_async.Body.t) call

  val save
    :  ?category:string
    -> id:[< `Link of Link.Id.t | `Comment of Comment.Id.t ]
    -> unit call

  val unsave : id:[< `Link of Link.Id.t | `Comment of Comment.Id.t ] -> unit call
  val saved_categories : (Cohttp.Response.t * Cohttp_async.Body.t) call

  val send_replies
    :  id:[< `Link of Link.Id.t | `Comment of Comment.Id.t ]
    -> enabled:bool
    -> unit call

  val set_contest_mode : link:Link.Id.t -> enabled:bool -> unit call

  val set_subreddit_sticky
    :  ?to_profile:bool
    -> link:Link.Id.t
    -> sticky_state:Sticky_state.t
    -> unit call

  val set_suggested_sort : link:Link.Id.t -> sort:Comment_sort.t option -> unit call
  val spoiler : link:Link.Id.t -> unit call
  val unspoiler : link:Link.Id.t -> unit call

  val store_visits
    :  links:Link.Id.t list
    -> (Cohttp.Response.t * Cohttp_async.Body.t) call

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
    -> kind:Link_kind.t
    -> (Link.Id.t * Uri.t) call

  val vote
    :  ?rank:int
    -> direction:Vote_direction.t
    -> target:[< `Link of Link.Id.t | `Comment of Comment.Id.t ]
    -> unit call

  val info
    :  ?subreddit:Subreddit_name.t
    -> Info_query.t
    -> [ `Comment of Thing.Comment.t
       | `Link of Thing.Link.t
       | `Subreddit of Thing.Subreddit.t
       ]
       Listing.t
       call

  (** Listings *)

  val best : (?include_categories:bool -> Link.t Listing.t call) with_listing_params
  val links_by_id : links:Link.Id.t list -> Link.t Listing.t call

  val comments
    :  ?subreddit:Subreddit_name.t
    -> ?comment:Comment.Id.t
    -> ?context:int
    -> ?depth:int
    -> ?limit:int
    -> ?showedits:bool
    -> ?showmore:bool
    -> ?sort:Comment_sort.t
    -> ?threaded:bool
    -> ?truncate:int
    -> link:Link.Id.t
    -> Comment_response.t call

  val duplicates
    : (?crossposts_only:bool
       -> ?sort:Duplicate_sort.t
       -> link:Link.Id.t
       -> Link.t Listing.t call)
      with_listing_params

  val hot
    : (?location:string
       -> ?include_categories:bool
       -> ?subreddit:Subreddit_name.t
       -> Link.t Listing.t call)
      with_listing_params

  val new_
    : (?include_categories:bool -> ?subreddit:Subreddit_name.t -> Link.t Listing.t call)
      with_listing_params

  val rising
    : (?include_categories:bool -> ?subreddit:Subreddit_name.t -> Link.t Listing.t call)
      with_listing_params

  val top
    : (?since:Historical_span.t
       -> ?include_categories:bool
       -> ?subreddit:Subreddit_name.t
       -> Link.t Listing.t call)
      with_listing_params

  val controversial
    : (?since:Historical_span.t
       -> ?include_categories:bool
       -> ?subreddit:Subreddit_name.t
       -> Link.t Listing.t call)
      with_listing_params

  val random : ?subreddit:Subreddit_name.t -> Link.Id.t call

  (** Private messages *)

  val block_author
    :  id:[< `Comment of Comment.Id.t | `Message of Message.Id.t ]
    -> unit call

  val collapse_message : messages:Message.Id.t list -> unit call
  val uncollapse_message : messages:Message.Id.t list -> unit call

  val compose_message
    :  ?g_recaptcha_response:string
    -> ?from_subreddit:Subreddit_name.t
    -> to_:Username.t
    -> subject:string
    -> text:string
    -> unit call

  val delete_message
    :  message:Message.Id.t
    -> (Cohttp.Response.t * Cohttp_async.Body.t) call

  val read_message : messages:Message.Id.t list -> unit call
  val unread_message : messages:Message.Id.t list -> unit call

  val inbox
    : (?include_categories:bool
       -> ?mid:string
       -> mark_read:bool
       -> [ `Comment of Comment.t | `Message of Message.t ] Listing.t call)
      with_listing_params

  val unread
    : (?include_categories:bool
       -> ?mid:string
       -> mark_read:bool
       -> [ `Comment of Comment.t | `Message of Message.t ] Listing.t call)
      with_listing_params

  val sent
    : (?include_categories:bool -> ?mid:string -> Message.t Listing.t call)
      with_listing_params

  val comment_replies
    : (?include_categories:bool
       -> ?mid:string
       -> mark_read:bool
       -> Comment.t Listing.t call)
      with_listing_params

  val subreddit_comments
    : (subreddit:Subreddit_name.t -> Comment.t Listing.t call) with_listing_params

  (** Moderation *)

  val log
    : (?mod_filter:Mod_filter.t
       -> ?subreddit:Subreddit_name.t
       -> ?type_:string
       -> Mod_action.t Listing.t call)
      with_listing_params

  val reports
    : (?location:string
       -> ?only:Links_or_comments.t
       -> ?subreddit:Subreddit_name.t
       -> [ `Link of Link.t | `Comment of Comment.t ] Listing.t call)
      with_listing_params

  val spam
    : (?location:string
       -> ?only:Links_or_comments.t
       -> ?subreddit:Subreddit_name.t
       -> [ `Link of Link.t | `Comment of Comment.t ] Listing.t call)
      with_listing_params

  val modqueue
    : (?location:string
       -> ?only:Links_or_comments.t
       -> ?subreddit:Subreddit_name.t
       -> [ `Link of Link.t | `Comment of Comment.t ] Listing.t call)
      with_listing_params

  val unmoderated
    : (?location:string
       -> ?only:Links_or_comments.t
       -> ?subreddit:Subreddit_name.t
       -> [ `Link of Link.t | `Comment of Comment.t ] Listing.t call)
      with_listing_params

  val edited
    : (?location:string
       -> ?only:Links_or_comments.t
       -> ?subreddit:Subreddit_name.t
       -> [ `Link of Link.t | `Comment of Comment.t ] Listing.t call)
      with_listing_params

  val accept_moderator_invite
    :  subreddit:Subreddit_name.t
    -> (Cohttp.Response.t * Cohttp_async.Body.t) call

  val approve : id:[< `Link of Link.Id.t | `Comment of Comment.Id.t ] -> unit call
  val remove : id:[< `Link of Link.Id.t | `Comment of Comment.Id.t ] -> unit call

  val distinguish
    :  ?sticky:bool
    -> id:[< `Link of Link.Id.t | `Comment of Comment.Id.t ]
    -> how:How_to_distinguish.t
    -> [> `Link of Link.t | `Comment of Comment.t ] call

  val ignore_reports : id:[< `Link of Link.Id.t | `Comment of Comment.Id.t ] -> unit call

  val unignore_reports
    :  id:[< `Link of Link.Id.t | `Comment of Comment.Id.t ]
    -> unit call

  val leavecontributor : subreddit:Subreddit.Id.t -> unit call
  val leavemoderator : subreddit:Subreddit.Id.t -> unit call
  val mute_message_author : message:Message.Id.t -> unit call
  val unmute_message_author : message:Message.Id.t -> unit call
  val stylesheet : subreddit:Subreddit_name.t -> Stylesheet.t call

  (** New modmail *)

  val create_modmail_conversation
    :  subject:string
    -> body:string
    -> subreddit:Subreddit_name.t
    -> to_:Username.t
    -> hide_author:bool
    -> Modmail.Conversation.t call

  (** Search *)

  val search
    : (?category:string
       -> ?include_facets:bool
       -> ?restrict_to_subreddit:Subreddit_name.t
       -> ?since:Historical_span.t
       -> ?sort:Search_sort.t
       -> ?types:Search_type.Set.t
       -> query:string
       -> (Thing.Link.t Listing.t option
          * [ `Subreddit of Thing.Subreddit.t | `User of Thing.User.t ] Listing.t option)
          call)
      with_listing_params

  (** Subreddits *)

  val banned
    : (?include_categories:bool
       -> ?user:Username.t
       -> subreddit:Subreddit_name.t
       -> Relationship.Ban.t Listing.t call)
      with_listing_params

  val muted
    : (?include_categories:bool
       -> ?user:Username.t
       -> subreddit:Subreddit_name.t
       -> Relationship.Mute.t Listing.t call)
      with_listing_params

  val wiki_banned
    : (?include_categories:bool
       -> ?user:Username.t
       -> subreddit:Subreddit_name.t
       -> Relationship.Ban.t Listing.t call)
      with_listing_params

  val contributors
    : (?include_categories:bool
       -> ?user:Username.t
       -> subreddit:Subreddit_name.t
       -> Relationship.Contributor.t Listing.t call)
      with_listing_params

  val wiki_contributors
    : (?include_categories:bool
       -> ?user:Username.t
       -> subreddit:Subreddit_name.t
       -> Relationship.Contributor.t Listing.t call)
      with_listing_params

  val moderators
    : (?include_categories:bool
       -> ?user:Username.t
       -> subreddit:Subreddit_name.t
       -> Relationship.Moderator.t Listing.t call)
      with_listing_params

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

  val subreddit_stylesheet
    :  ?reason:string
    -> operation:Stylesheet_operation.t
    -> stylesheet_contents:string
    -> subreddit:Subreddit_name.t
    -> (Cohttp.Response.t * Cohttp_async.Body.t) call

  val subscribe
    :  ?skip_initial_defaults:bool
    -> action:Subscription_action.t
    -> (Cohttp.Response.t * Cohttp_async.Body.t) call

  val upload_sr_img
    :  ?form_id:string
    -> file:string
    -> header:bool
    -> image_type:Image_type.t
    -> name:string
    -> subreddit:Subreddit_name.t
    -> upload_type:Upload_type.t
    -> (Cohttp.Response.t * Cohttp_async.Body.t) call

  val search_profiles
    : (?sort:Subreddit_search_sort.t
       -> query:string
       -> (Cohttp.Response.t * Cohttp_async.Body.t) call)
      with_listing_params

  val about_subreddit : subreddit:Subreddit_name.t -> Subreddit.t call

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

  val get_subreddits
    : (?include_categories:bool
       -> relationship:Subreddit_relationship.t
       -> (Cohttp.Response.t * Cohttp_async.Body.t) call)
      with_listing_params

  val search_subreddits
    : (?show_users:bool
       -> ?sort:Subreddit_search_sort.t
       -> query:string
       -> (Cohttp.Response.t * Cohttp_async.Body.t) call)
      with_listing_params

  val list_subreddits
    : (?include_categories:bool
       -> ?show_users:bool
       -> sort:Subreddit_listing_sort.t
       -> (Cohttp.Response.t * Cohttp_async.Body.t) call)
      with_listing_params

  (** Users *)

  val about_user : username:Username.t -> User.t call

  val list_user_subreddits
    : (?include_categories:bool
       -> sort:User_subreddit_sort.t
       -> Subreddit.t Listing.t call)
      with_listing_params

  val add_relationship
    :  relationship:Relationship_spec.t
    -> username:Username.t
    -> duration:Relationship_spec.Duration.t
    -> ?subreddit:Subreddit_name.t
    -> ?note:string
    -> ?ban_reason:string
    -> ?ban_message:string
    -> ?ban_context:string
    -> unit call

  val remove_relationship
    :  relationship:Relationship_spec.t
    -> username:Username.t
    -> ?subreddit:Subreddit_name.t
    -> unit call

  (** Wiki *)

  val add_or_remove_wiki_editor
    :  add_or_remove:Add_or_remove.t
    -> page:Wiki_page.Id.t
    -> user:Username.t
    -> (Cohttp.Response.t * Cohttp_async.Body.t) call

  val edit_wiki_page
    :  ?previous:Uuid.t
    -> ?reason:string
    -> content:string
    -> page:Wiki_page.Id.t
    -> (unit, Wiki_page.Edit_conflict.t) Result.t call

  val toggle_wiki_revision_visibility
    :  page:Wiki_page.Id.t
    -> revision:string
    -> (Cohttp.Response.t * Cohttp_async.Body.t) call

  val revert_wiki_page
    :  page:Wiki_page.Id.t
    -> revision:string
    -> (Cohttp.Response.t * Cohttp_async.Body.t) call

  val wiki_discussions
    : (page:Wiki_page.Id.t -> (Cohttp.Response.t * Cohttp_async.Body.t) call)
      with_listing_params

  val wiki_pages
    :  ?subreddit:Subreddit_name.t
    -> (Cohttp.Response.t * Cohttp_async.Body.t) call

  val subreddit_wiki_revisions
    : (?subreddit:Subreddit_name.t -> (Cohttp.Response.t * Cohttp_async.Body.t) call)
      with_listing_params

  val wiki_page_revisions
    : (page:Wiki_page.Id.t -> (Cohttp.Response.t * Cohttp_async.Body.t) call)
      with_listing_params

  val wiki_permissions
    :  page:Wiki_page.Id.t
    -> (Cohttp.Response.t * Cohttp_async.Body.t) call

  val set_wiki_permissions
    :  listed:bool
    -> page:Wiki_page.Id.t
    -> permission_level:int
    -> (Cohttp.Response.t * Cohttp_async.Body.t) call

  val wiki_page
    :  ?compare_revisions:string option * string option
    -> page:Wiki_page.Id.t
    -> Wiki_page.t call
end

module type Api_error = sig
  type t =
    | Cohttp_raised of Exn.t
    | Reddit_reported_error of Cohttp.Response.t * Cohttp_async.Body.t
  [@@deriving sexp_of]
end

module type Api = sig
  module Api_error : Api_error
  include S with type 'a response := ('a, Api_error.t) Result.t

  module Raw :
    S
      with type _ response := (Cohttp.Response.t * Cohttp_async.Body.t, Exn.t) Result.t
      with module Parameters := Parameters

  module Exn : S with type 'a response := 'a with module Parameters := Parameters
end
