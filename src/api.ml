open! Core
open! Async

type 'a call =
  ?param_list_override:((string * string sexp_list) sexp_list
                        -> (string * string sexp_list) sexp_list)
  -> Connection.t
  -> 'a Deferred.t

let call_api ?(param_list_override = Fn.id) connection ~endpoint ~http_verb ~params =
  let params = param_list_override params in
  let uri = sprintf "https://oauth.reddit.com%s" endpoint |> Uri.of_string in
  match http_verb with
  | `GET ->
    let uri = Uri.add_query_params uri params in
    Connection.get connection uri
  | `POST -> Connection.post_form connection uri ~params
;;

let get = call_api ~http_verb:`GET
let post = call_api ~http_verb:`POST

module Param_dsl = struct
  type t = (string * string list) list

  let required f key values : t = [ key, List.map values ~f ]
  let required' f key value : t = [ key, [ f value ] ]

  let optional f key values_opt : t =
    Option.to_list values_opt |> List.bind ~f:(required f key)
  ;;

  let optional' f key value_opt : t =
    Option.to_list value_opt |> List.bind ~f:(required' f key)
  ;;

  let include_optional f value_opt : t = Option.to_list value_opt |> List.bind ~f
  let _ = optional
  let combine = List.join
  let bool = Bool.to_string
  let string = Fn.id
  let int = Int.to_string
  let fullname_ = Fullname.to_string
  let username_ = Username.to_string
  let json = Yojson.Safe.to_string
  let time = Time.to_string_iso8601_basic ~zone:Time.Zone.utc
end

let simple_post_fullnames_as_id endpoint ~fullnames =
  let endpoint = sprintf "/api/%s" endpoint in
  post ~endpoint ~params:Param_dsl.(required fullname_ "id" fullnames)
;;

let simple_post_fullname_as_id ~fullname =
  simple_post_fullnames_as_id ~fullnames:[ fullname ]
;;

let simple_toggle verb =
  simple_post_fullnames_as_id verb, simple_post_fullnames_as_id ("un" ^ verb)
;;

let simple_toggle' verb =
  simple_post_fullname_as_id verb, simple_post_fullname_as_id ("un" ^ verb)
;;

module Listing_params = struct
  module Pagination = struct
    module Before_or_after = struct
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

    let params_of_t { before_or_after; index; count } =
      [ ( (match before_or_after with
          | Before -> "before"
          | After -> "after")
        , [ Fullname.to_string index ] )
      ; "count", [ Int.to_string count ]
      ]
    ;;
  end

  type t =
    { pagination : Pagination.t option
    ; limit : int option
    ; show_all : bool
    }
  [@@deriving sexp]

  let params_of_t { pagination; limit; show_all } =
    let open Param_dsl in
    combine
      [ include_optional Pagination.params_of_t pagination
      ; optional' int "limit" limit
      ; optional' string "show" (Option.some_if show_all "all")
      ]
  ;;
end

let api_type : Param_dsl.t = [ "api_type", [ "json" ] ]
let me = get ~endpoint:"/api/v1/me" ~params:[]
let karma = get ~endpoint:"/api/v1/me/karma" ~params:[]
let trophies = get ~endpoint:"/api/v1/me/trophies" ~params:[]
let needs_captcha = get ~endpoint:"/api/v1/me/needs_captcha" ~params:[]

let prefs which ~listing_params ~subreddit_detail ~include_categories =
  let endpoint = sprintf "/api/%s" which in
  let params =
    let open Param_dsl in
    combine
      [ Listing_params.params_of_t listing_params
      ; optional' bool "sr_detail" subreddit_detail
      ; optional' bool "include_categories" include_categories
      ]
  in
  post ~endpoint ~params
;;

let friends = prefs "friends"
let blocked = prefs "blocked"
let messaging = prefs "messaging"
let trusted = prefs "trusted"

let add_comment ?return_rtjson ?richtext_json ~parent ~text =
  let endpoint = "/api/comment" in
  let params =
    let open Param_dsl in
    combine
      [ api_type
      ; required' fullname_ "thing_id" parent
      ; required' string "text" text
      ; optional' bool "return_rtjson" return_rtjson
      ; optional' json "richtext_json" richtext_json
      ]
  in
  post ~endpoint ~params
;;

let delete ~fullname =
  let endpoint = "/api/comment" in
  let params =
    let open Param_dsl in
    Param_dsl.combine [ required' fullname_ "id" fullname ]
  in
  post ~endpoint ~params
;;

let edit ?return_rtjson ?richtext_json ~fullname ~text =
  let endpoint = "/api/editusertext" in
  let params =
    let open Param_dsl in
    combine
      [ api_type
      ; required' fullname_ "thing_id" fullname
      ; required' string "text" text
      ; optional' bool "return_rtjson" return_rtjson
      ; optional' json "richtext_json" richtext_json
      ]
  in
  post ~endpoint ~params
;;

let follow ~submission ~follow =
  let endpoint = "/api/follow_post" in
  let params =
    let open Param_dsl in
    combine [ required' fullname_ "fullname" submission; required' bool "follow" follow ]
  in
  post ~endpoint ~params
;;

let hide, unhide =
  let hide', unhide' = simple_toggle "hide" in
  ( (fun ~submissions -> hide' ~fullnames:submissions)
  , fun ~submissions -> unhide' ~fullnames:submissions )
;;

module Info_query = struct
  type t =
    | Id of Fullname.t list
    | Url of Uri_sexp.t
  [@@deriving sexp]

  let params_of_t t =
    match t with
    | Id fullnames -> [ "id", List.map fullnames ~f:Fullname.to_string ]
    | Url uri -> [ "url", [ Uri.to_string uri ] ]
  ;;
end

let info ?subreddit query =
  let endpoint =
    let subreddit_part =
      Option.value_map subreddit ~default:"" ~f:(sprintf !"/r/%{Subreddit_name}")
    in
    sprintf !"%s/api/info" subreddit_part
  in
  let params = Info_query.params_of_t query in
  get ~endpoint ~params
;;

let lock, unlock = simple_toggle' "lock"
let mark_nsfw, unmark_nsfw = simple_toggle' "marknsfw"

module Comment_sort = struct
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

  let to_string t =
    match t with
    | Q_and_a -> "qa"
    | _ -> sexp_of_t t |> Sexp.to_string |> String.lowercase
  ;;
end

(* TODO: mutex *)
let more_children ?id ?limit_children ~submission ~children ~sort =
  let endpoint = "/api/morechildren" in
  let params =
    let open Param_dsl in
    combine
      [ api_type
      ; required Id36.Comment.to_string "children" children
      ; required' fullname_ "link_id" submission
      ; optional' Id36.More_children.to_string "id" id
      ; optional' bool "limit_children" limit_children
      ; required' Comment_sort.to_string "sort" sort
      ]
  in
  get ~endpoint ~params
;;

module Report_target = struct
  type t =
    | Modmail_conversation of Id36.Modmail_conversation.t
    | Fullname of Fullname.t
  [@@deriving sexp]

  let params_of_t t =
    match t with
    | Modmail_conversation id ->
      [ "modmail_conv_id", [ Id36.Modmail_conversation.to_string id ] ]
    | Fullname fullname -> [ "thing_id", [ Fullname.to_string fullname ] ]
  ;;
end

let report
    ?from_modmail
    ?from_help_desk
    ?additional_info
    ?custom_text
    ?other_reason
    ?rule_reason
    ?site_reason
    ?sr_name
    ~target
    ~reason
  =
  let endpoint = "/api/report" in
  let params =
    let open Param_dsl in
    combine
      [ Report_target.params_of_t target
      ; api_type
      ; required' string "reason" reason
      ; optional' string "additional_info" additional_info
      ; optional' string "custom_text" custom_text
      ; optional' string "other_reason" other_reason
      ; optional' string "rule_reason" rule_reason
      ; optional' string "site_reason" site_reason
      ; optional' string "sr_name" sr_name
      ; optional' bool "from_modmail" from_modmail
      ; optional' bool "from_help_desk" from_help_desk
      ]
  in
  post ~endpoint ~params
;;

let report_award ~award_id =
  let endpoint = "/api/report_award" in
  let params = [ "award_id", [ award_id ] ] in
  post ~endpoint ~params
;;

let save ?category ~fullname =
  let endpoint = "/api/save" in
  let params =
    let open Param_dsl in
    combine [ required' fullname_ "id" fullname; optional' string "category" category ]
  in
  post ~endpoint ~params
;;

let unsave ~fullname =
  let endpoint = "/api/save" in
  let params = [ "id", [ Fullname.to_string fullname ] ] in
  post ~endpoint ~params
;;

let saved_categories = get ~endpoint:"/api/saved_categories" ~params:[]

let send_replies ~fullname ~enabled =
  let endpoint = "/api/sendreplies" in
  let params =
    let open Param_dsl in
    combine [ required' fullname_ "id" fullname; required' bool "state" enabled ]
  in
  post ~endpoint ~params
;;

let set_contest_mode ~fullname ~enabled =
  let endpoint = "/api/set_contest_mode" in
  let params =
    let open Param_dsl in
    combine
      [ api_type; required' fullname_ "id" fullname; required' bool "state" enabled ]
  in
  post ~endpoint ~params
;;

module Sticky_state = struct
  type t =
    | Sticky of { slot : int }
    | Unsticky
  [@@deriving sexp]

  let params_of_t t =
    match t with
    | Sticky { slot } ->
      [ "state", [ Bool.to_string true ]; "num", [ Int.to_string slot ] ]
    | Unsticky -> [ "state", [ Bool.to_string false ] ]
  ;;
end

let set_subreddit_sticky ?to_profile ~fullname ~sticky_state =
  let endpoint = "/api/set_subreddit_sticky" in
  let params =
    let open Param_dsl in
    combine
      [ Sticky_state.params_of_t sticky_state
      ; api_type
      ; required' fullname_ "id" fullname
      ; optional' bool "to_profile" to_profile
      ]
  in
  post ~endpoint ~params
;;

let set_suggested_sort ~fullname ~sort =
  let endpoint = "/api/set_suggested_sort" in
  let params =
    let open Param_dsl in
    combine
      [ api_type
      ; required' fullname_ "id" fullname
      ; required'
          string
          "sort"
          (Option.value_map sort ~f:Comment_sort.to_string ~default:"blank")
      ]
  in
  post ~endpoint ~params
;;

let spoiler, unspoiler = simple_toggle' "spoiler"

let store_visits ~submissions =
  let endpoint = "/api/store_visits" in
  let params =
    let open Param_dsl in
    combine [ required Fullname.to_string "links" submissions ]
  in
  post ~endpoint ~params
;;

module Submission_kind = struct
  module Self_post_body = struct
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

  let params_of_t t =
    match t with
    | Link { url } -> [ "kind", [ "link" ]; "url", [ url ] ]
    | Self body ->
      [ "kind", [ "link" ]
      ; ( "text"
        , [ (match body with
            | Markdown markdown -> markdown
            | Richtext_json json -> Yojson.Safe.to_string json)
          ] )
      ]
    (* TODO Should we disallow these? *)
    | Image | Video | Videogif -> assert false
  ;;
end

let submit
    ?ad
    ?nsfw
    ?resubmit
    ?sendreplies
    ?spoiler
    ?flair_id
    ?flair_text
    ?collection_id
    ?event_start
    ?event_end
    ?event_tz
    ~subreddit
    ~title
    ~kind
  =
  let endpoint = "/api/store_visits" in
  let params =
    let open Param_dsl in
    combine
      [ Submission_kind.params_of_t kind
      ; api_type
      ; required' Subreddit_name.to_string "sr" subreddit
      ; required' string "title" title
      ; optional' bool "ad" ad
      ; optional' bool "nsfw" nsfw
      ; optional' bool "resubmit" resubmit
      ; optional' bool "sendreplies" sendreplies
      ; optional' bool "spoiler" spoiler
      ; (* TODO Do these have to go together? *)
        optional' string "flair_id" flair_id
      ; optional' string "flair_text" flair_text
      ; optional' string "collection_id" collection_id
      ; optional' time "event_start" event_start
      ; optional' time "event_end" event_end
      ; optional' string "event_tz" event_tz
      ]
  in
  post ~endpoint ~params
;;

module Vote_direction = struct
  type t =
    | Up
    | Neutral
    | Down
  [@@deriving sexp]

  let int_of_t t =
    match t with
    | Up -> 1
    | Neutral -> 0
    | Down -> -1
  ;;

  let params_of_t t = [ "dir", [ int_of_t t |> Int.to_string ] ]
end

let vote ?rank ~direction ~fullname =
  let endpoint = "/api/vote" in
  let params =
    let open Param_dsl in
    combine
      [ Vote_direction.params_of_t direction
      ; required' fullname_ "fullname" fullname
      ; optional' int "rank" rank
      ]
  in
  post ~endpoint ~params
;;

let trending_subreddits = get ~endpoint:"/api/trending_subreddits" ~params:[]

let best ?include_categories ?subreddit_detail ~listing_params =
  let endpoint = "/best" in
  let params =
    let open Param_dsl in
    combine
      [ Listing_params.params_of_t listing_params
      ; optional' bool "include_categories" include_categories
      ; optional' bool "sr_detail" subreddit_detail
      ]
  in
  get ~endpoint ~params
;;

let by_id ~fullnames =
  let endpoint =
    List.map fullnames ~f:Fullname.to_string
    |> String.concat ~sep:","
    |> sprintf "/by_id/%s"
  in
  get ~endpoint ~params:[]
;;

let comments
    ?subreddit
    ?comment
    ?context
    ?depth
    ?limit
    ?showedits
    ?showmore
    ?sort
    ?subreddit_detail
    ?threaded
    ?truncate
    ~submission
  =
  let endpoint =
    let subreddit_part =
      Option.value_map subreddit ~default:"" ~f:(fun name ->
          sprintf !"/r/%{Subreddit_name}" name)
    in
    sprintf !"%s/comments/%{Id36.Submission}" subreddit_part submission
  in
  let params =
    let open Param_dsl in
    combine
      [ optional' Id36.Comment.to_string "comment" comment
      ; optional' int "context" context
      ; optional' int "depth" depth
      ; optional' int "limit" limit
      ; optional' bool "showedits" showedits
      ; optional' bool "showmore" showmore
      ; optional' Comment_sort.to_string "sort" sort
      ; optional' bool "sr_detail" subreddit_detail
      ; optional' bool "threaded" threaded
      ; optional' int "truncate" truncate
      ]
  in
  get ~endpoint ~params
;;

module Duplicate_sort = struct
  type t =
    | Number_of_comments
    | New

  let params_of_t t =
    [ ( "sort"
      , match t with
        | Number_of_comments -> [ "number_of_comments" ]
        | New -> [ "new" ] )
    ]
  ;;
end

let duplicates ?crossposts_only ?subreddit_detail ?sort ~submission_id ~listing_params =
  let endpoint = sprintf !"/duplicates/%{Id36.Submission}" submission_id in
  let params =
    let open Param_dsl in
    combine
      [ Listing_params.params_of_t listing_params
      ; optional' bool "crossposts_only" crossposts_only
      ; optional' bool "sr_detail" subreddit_detail
      ; include_optional Duplicate_sort.params_of_t sort
      ]
  in
  get ~endpoint ~params
;;

module Historical_span = struct
  module T = struct
    type t =
      | Hour
      | Day
      | Week
      | Month
      | Year
      | All
    [@@deriving sexp]
  end

  include T
  include Sexpable.To_stringable (T)

  let params_of_t t = [ "t", [ to_string t |> String.lowercase ] ]
end

let basic_post_listing
    ?include_categories
    ?subreddit_detail
    ?subreddit
    ~listing_params
    ~endpoint_part
    ~extra_params
  =
  let endpoint =
    let subreddit_part =
      Option.value_map subreddit ~default:"" ~f:(fun name ->
          sprintf !"/r/%{Subreddit_name}" name)
    in
    sprintf !"%s/%s" subreddit_part endpoint_part
  in
  let params =
    let open Param_dsl in
    combine
      [ Listing_params.params_of_t listing_params
      ; optional' bool "include_categories" include_categories
      ; optional' bool "sr_detail" subreddit_detail
      ; extra_params
      ]
  in
  get ~endpoint ~params
;;

let hot ?location =
  let extra_params =
    let open Param_dsl in
    optional' string "location" location
  in
  basic_post_listing ~endpoint_part:"hot" ~extra_params
;;

let new_ = basic_post_listing ~endpoint_part:"new" ~extra_params:[]
let rising = basic_post_listing ~endpoint_part:"rising" ~extra_params:[]

let top ?since =
  let extra_params = Param_dsl.include_optional Historical_span.params_of_t since in
  basic_post_listing ~endpoint_part:"top" ~extra_params
;;

let controversial ?since =
  let extra_params = Param_dsl.include_optional Historical_span.params_of_t since in
  basic_post_listing ~endpoint_part:"controversial" ~extra_params
;;

let random ?subreddit =
  let endpoint =
    let subreddit_part =
      Option.value_map subreddit ~default:"" ~f:(fun name ->
          sprintf !"/r/%{Subreddit_name}" name)
    in
    sprintf !"%s/random" subreddit_part
  in
  get ~endpoint ~params:[]
;;

let block = simple_post_fullname_as_id "block"
let collapse_message, uncollapse_message = simple_toggle "collapse_message,"

let compose ?g_recaptcha_response ?from_subreddit ~to_ ~subject ~text =
  let endpoint = "/api/compose" in
  let params =
    let open Param_dsl in
    combine
      [ api_type
      ; optional' string "g-recaptcha-response" g_recaptcha_response
      ; optional' Subreddit_name.to_string "from_sr" from_subreddit
      ; required' username_ "to" to_
      ; required' string "subject" subject
      ; required' string "text" text
      ]
  in
  post ~endpoint ~params
;;

let delete_message = simple_post_fullname_as_id "del_msg"
let read_message, unread_message = simple_toggle "read_message"
let unblock_subreddit = simple_post_fullnames_as_id "unblock_subreddit"

let message_listing
    ?include_categories
    ?mid
    ?subreddit_detail
    ~listing_params
    ~mark_read
    endpoint
  =
  let endpoint = "/message/" ^ endpoint in
  let params =
    let open Param_dsl in
    combine
      [ Listing_params.params_of_t listing_params
      ; optional' bool "include_categories" include_categories
      ; optional' string "mid" mid
      ; optional' bool "sr_detail" subreddit_detail
      ; required bool "mark" mark_read
      ]
  in
  get ~endpoint ~params
;;

let inbox = message_listing "inbox"
let unread = message_listing "unread"
let sent = message_listing "sent"

module Mod_filter = struct
  type t =
    | Moderators of Username.t list
    | Admin

  let params_of_t t =
    [ ( "mod"
      , match t with
        | Admin -> [ "a" ]
        | Moderators moderators -> List.map moderators ~f:Username.to_string )
    ]
  ;;
end

let log ?mod_filter ?subreddit_detail ?subreddit ?type_ ~listing_params =
  let endpoint =
    let subreddit_part =
      Option.value_map subreddit ~default:"" ~f:(fun name ->
          sprintf !"/r/%{Subreddit_name}" name)
    in
    sprintf !"%s/about/log" subreddit_part
  in
  let params =
    let open Param_dsl in
    combine
      [ Listing_params.params_of_t listing_params
      ; include_optional Mod_filter.params_of_t mod_filter
      ; optional' bool "sr_detail" subreddit_detail
      ; optional' string "type" type_
      ]
  in
  get ~endpoint ~params
;;

module Links_or_comments = struct
  type t =
    | Links
    | Comments

  let params_of_t t =
    [ ( "only"
      , match t with
        | Links -> [ "links" ]
        | Comments -> [ "comments" ] )
    ]
  ;;
end

let mod_listing ?location ?only ?subreddit ?subreddit_detail ~listing_params ~endpoint =
  let endpoint =
    let subreddit_part =
      Option.value_map subreddit ~default:"" ~f:(fun name ->
          sprintf !"/r/%{Subreddit_name}" name)
    in
    sprintf !"%s/about/%s" subreddit_part endpoint
  in
  let params =
    let open Param_dsl in
    combine
      [ Listing_params.params_of_t listing_params
      ; include_optional Links_or_comments.params_of_t only
      ; optional' bool "sr_detail" subreddit_detail
      ; optional' string "location" location
      ]
  in
  get ~endpoint ~params
;;

let reports = mod_listing ~endpoint:"reports"
let spam = mod_listing ~endpoint:"spam"
let modqueue = mod_listing ~endpoint:"modqueue"
let unmoderated = mod_listing ~endpoint:"unmoderated"
let edited = mod_listing ~endpoint:"edited"

let accept_moderator_invite ~subreddit =
  let endpoint = sprintf !"/r/%{Subreddit_name}/api/accept_moderator_invite" subreddit in
  post ~endpoint ~params:api_type
;;

let approve = simple_post_fullname_as_id "approve"
let remove = simple_post_fullname_as_id "remove"

module How_to_distinguish = struct
  type t =
    | Mod
    | Admin
    | Special
    | Undistinguish

  let params_of_t t =
    [ ( "how"
      , [ (match t with
          | Mod -> "yes"
          | Admin -> "admin"
          | Special -> "special"
          | Undistinguish -> "no")
        ] )
    ]
  ;;
end

let distinguish ?sticky ~fullname ~how =
  let endpoint = "/api/distinguish" in
  let params =
    let open Param_dsl in
    combine
      [ How_to_distinguish.params_of_t how
      ; required' fullname_ "id" fullname
      ; optional' bool "sticky" sticky
      ]
  in
  post ~endpoint ~params
;;

let ignore_reports, unignore_reports = simple_toggle' "ignore_reports"
let leavecontributor = simple_post_fullname_as_id "leavecontributor"
let leavemoderator = simple_post_fullname_as_id "leavemoderator"
let mute_message_author, unmute_message_author = simple_toggle' "mute_message_author"

let stylesheet ~subreddit =
  let endpoint = sprintf !"/r/%{Subreddit_name}/stylesheet" subreddit in
  get ~endpoint ~params:[]
;;

module Relationship = struct
  module Duration = struct
    type t =
      | Permanent
      | Days of int
    [@@deriving sexp]

    let params_of_t t =
      [ ( "duration"
        , match t with
          | Permanent -> []
          | Days days -> [ Int.to_string days ] )
      ]
    ;;
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

  let to_string t =
    match t with
    | Friend -> "friend"
    | Moderator -> "moderator"
    | Moderator_invite -> "moderator_invite"
    | Contributor -> "contributor"
    | Banned -> "banned"
    | Muted -> "muted"
    | Wiki_banned -> "wikibanned"
    | Wiki_contributor -> "wikicontributor"
  ;;

  let params_of_t t = [ "type", [ to_string t ] ]
end

let add_relationship
    ~relationship
    ~username
    ~subreddit
    ~duration
    ?note
    ?ban_reason
    ?ban_message
    ?ban_context
  =
  let endpoint = sprintf !"/r/%{Subreddit_name}/api/unfriend" subreddit in
  let params =
    Relationship.Duration.params_of_t duration
    @ Relationship.params_of_t relationship
    @
    let open Param_dsl in
    combine
      [ Relationship.Duration.params_of_t duration
      ; Relationship.params_of_t relationship
      ; api_type
      ; required' username_ "name" username
      ; optional' string "note" note
      ; optional' string "ban_reason" ban_reason
      ; optional' string "ban_message" ban_message
      ; optional' fullname_ "ban_context" ban_context
      ]
  in
  post ~endpoint ~params
;;

let remove_relationship ~relationship ~username ~subreddit =
  let endpoint = sprintf !"/r/%{Subreddit_name}/api/unfriend" subreddit in
  let params =
    let open Param_dsl in
    combine
      [ Relationship.params_of_t relationship
      ; api_type
      ; required' username_ "name" username
      ]
  in
  post ~endpoint ~params
;;
