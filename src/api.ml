open! Core
open! Async
open Thing

module Parameters = struct
  module Pagination = struct
    type t =
      | Before of Listing.Page_id.t
      | After of Listing.Page_id.t
    [@@deriving sexp]

    let params_of_t t =
      let key =
        match t with
        | Before _ -> "before"
        | After _ -> "after"
      in
      let value =
        match t with
        | Before page_id | After page_id -> Listing.Page_id.to_string page_id
      in
      [ key, [ value ] ]
    ;;
  end

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

  module Report_target = struct
    type t = Fullname.t [@@deriving sexp]

    let params_of_t t =
      match t with
      | `Modmail_conversation id ->
        [ "modmail_conv_id", [ Modmail_conversation.Id36.to_string id ] ]
      | _ -> [ "thing_id", [ Fullname.to_string t ] ]
    ;;
  end

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

  module Link_kind = struct
    module Self_post_body = struct
      type t =
        | Markdown of string
        | Richtext_json of Json.t
      [@@deriving sexp]
    end

    type t =
      | Link of { url : string }
      | Self of Self_post_body.t
    [@@deriving sexp]

    let params_of_t t =
      match t with
      | Link { url } -> [ "kind", [ "link" ]; "url", [ url ] ]
      | Self body ->
        [ "kind", [ "link" ]
        ; ( "text"
          , [ (match body with
              | Markdown markdown -> markdown
              | Richtext_json json -> Json.to_string json)
            ] )
        ]
    ;;
  end

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

  module Info_query = struct
    type t =
      | Id of [ Fullname.link | Fullname.comment | Fullname.subreddit ] list
      | Url of Uri_sexp.t
    [@@deriving sexp]

    let params_of_t t =
      match t with
      | Id fullnames -> [ "id", List.map fullnames ~f:Fullname.to_string ]
      | Url uri -> [ "url", [ Uri.to_string uri ] ]
    ;;
  end

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

  module Search_sort = struct
    type t =
      | Relevance
      | Hot
      | Top
      | New
      | Comments

    let params_of_t t =
      [ ( "sort"
        , [ (match t with
            | Relevance -> "relevance"
            | Hot -> "hot"
            | Top -> "top"
            | New -> "new"
            | Comments -> "comments")
          ] )
      ]
    ;;
  end

  module Search_type = struct
    module T = struct
      type t =
        | Subreddit
        | Link
        | User
      [@@deriving compare, sexp]
    end

    include T
    include Comparable.Make (T)

    let to_string t =
      match t with
      | Subreddit -> "sr"
      | Link -> "link"
      | User -> "user"
    ;;
  end

  module Link_type = struct
    type t =
      | Any
      | Link
      | Self

    let params_of_t t =
      [ ( "link_type"
        , [ (match t with
            | Any -> "any"
            | Link -> "link"
            | Self -> "self")
          ] )
      ]
    ;;
  end

  module Spam_level = struct
    type t =
      | Low
      | High
      | All

    let to_string t =
      match t with
      | Low -> "low"
      | High -> "high"
      | All -> "all"
    ;;
  end

  module Subreddit_type = struct
    type t =
      | Gold_restricted
      | Archived
      | Restricted
      | Employees_only
      | Gold_only
      | Private
      | User
      | Public

    let to_string t =
      match t with
      | Gold_restricted -> "gold_restricted"
      | Archived -> "archived"
      | Restricted -> "restricted"
      | Employees_only -> "employees_only"
      | Gold_only -> "gold_only"
      | Private -> "private"
      | User -> "user"
      | Public -> "public"
    ;;
  end

  module Wiki_mode = struct
    type t =
      | Disabled
      | Mod_only
      | Anyone

    let to_string t =
      match t with
      | Disabled -> "disabled"
      | Mod_only -> "modonly"
      | Anyone -> "anyone"
    ;;
  end

  module Stylesheet_operation = struct
    type t =
      | Save
      | Preview
    [@@deriving sexp]

    let to_string t =
      match t with
      | Save -> "save"
      | Preview -> "preview"
    ;;
  end

  module Subscription_action = struct
    type t =
      | Subscribe
      | Unsubscribe

    let to_string t =
      match t with
      | Subscribe -> "sub"
      | Unsubscribe -> "unsub"
    ;;
  end

  module Image_type = struct
    type t =
      | Png
      | Jpg

    let to_string t =
      match t with
      | Png -> "png"
      | Jpg -> "jpg"
    ;;
  end

  module Upload_type = struct
    type t =
      | Image
      | Header
      | Icon
      | Banner

    let to_string t =
      match t with
      | Image -> "img"
      | Header -> "header"
      | Icon -> "icon"
      | Banner -> "banner"
    ;;
  end

  module Subreddit_search_sort = struct
    type t =
      | Relevance
      | Activity

    let to_string t =
      match t with
      | Relevance -> "relevance"
      | Activity -> "activity"
    ;;
  end

  module Subreddit_relationship = struct
    type t =
      | Subscriber
      | Contributor
      | Moderator
      | Stream_subscriber

    let to_string t =
      match t with
      | Subscriber -> "subscriber"
      | Contributor -> "contributor"
      | Moderator -> "moderator"
      | Stream_subscriber -> "streams"
    ;;
  end

  module Subreddit_listing_sort = struct
    type t =
      | Popular
      | New
      | Gold
      | Default

    let to_string t =
      match t with
      | Popular -> "popular"
      | New -> "new"
      | Gold -> "gold"
      | Default -> "default"
    ;;
  end

  module User_subreddit_sort = struct
    type t =
      | Popular
      | New

    let to_string t =
      match t with
      | Popular -> "popular"
      | New -> "new"
    ;;
  end

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

  module Add_or_remove = struct
    type t =
      | Add
      | Remove

    let to_string t =
      match t with
      | Add -> "add"
      | Remove -> "del"
    ;;
  end
end

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
  let json = Json.to_string
  let time = Time.to_string_iso8601_basic ~zone:Time.Zone.utc
end

module Make (Connection : sig
  type t

  val get : t -> Uri.t -> (Cohttp.Response.t * Cohttp_async.Body.t) Deferred.t

  val post_form
    :  t
    -> Uri.t
    -> params:(string * string list) list
    -> (Cohttp.Response.t * Cohttp_async.Body.t) Deferred.t

  val more_children_sequencer : t -> unit Sequencer.t
end) (Output : sig
  type 'a t

  val finalize
    :  (Cohttp.Response.t * Cohttp_async.Body.t -> 'a Deferred.Option.t)
    -> Cohttp.Response.t * Cohttp_async.Body.t
    -> 'a t Deferred.t
end) =
struct
  module Connection = Connection

  let call_api k ?(param_list_override = Fn.id) connection ~endpoint ~http_verb ~params =
    let k = Output.finalize k in
    let params = ("raw_json", [ "1" ]) :: params in
    let params = param_list_override params in
    let uri = sprintf "https://oauth.reddit.com%s" endpoint |> Uri.of_string in
    match http_verb with
    | `GET ->
      let uri = Uri.add_query_params uri params in
      Connection.get connection uri >>= k
    | `POST -> Connection.post_form connection uri ~params >>= k
  ;;

  let get = call_api ~http_verb:`GET
  let post = call_api ~http_verb:`POST

  let optional_subreddit_endpoint ?subreddit suffix =
    let subreddit_part =
      Option.value_map subreddit ~default:"" ~f:(sprintf !"/r/%{Subreddit_name}")
    in
    sprintf !"%s%s" subreddit_part suffix
  ;;

  let simple_post_fullnames_as_id endpoint ~fullnames =
    let endpoint = sprintf "/api/%s" endpoint in
    post ~endpoint ~params:Param_dsl.(required fullname_ "id" fullnames)
  ;;

  let simple_post_fullname_as_id ~fullname =
    simple_post_fullnames_as_id ~fullnames:[ fullname ]
  ;;

  let simple_toggle verb k =
    simple_post_fullnames_as_id verb k, simple_post_fullnames_as_id ("un" ^ verb) k
  ;;

  let simple_toggle' verb k =
    simple_post_fullname_as_id verb k, simple_post_fullname_as_id ("un" ^ verb) k
  ;;

  let api_type : Param_dsl.t = [ "api_type", [ "json" ] ]

  open Parameters
  open Deferred.Option.Let_syntax

  let some = Deferred.map ~f:Option.some
  let none = Deferred.return None

  let result_of_response (response, body) =
    match Cohttp.Response.status response with
    | #Cohttp.Code.success_status -> return (response, body)
    | _ -> none
  ;;

  let handle_json_response f response =
    let%bind _response, body = result_of_response response in
    let%bind body_string = Cohttp_async.Body.to_string body |> some in
    Json.of_string body_string |> f |> return
  ;;

  let ignore_success_response response =
    let%bind (_ : Cohttp.Response.t * Cohttp_async.Body.t) =
      result_of_response response
    in
    return ()
  ;;

  let ignore_empty_object =
    handle_json_response (fun json ->
        match json with
        | `Object [] -> ()
        | _ -> raise_s [%message "Unexpected JSON response" (json : Json.t)])
  ;;

  let link_of_json json =
    let thing = Thing.of_json json in
    match thing with
    | `Link _ as thing -> thing
    | _ -> raise_s [%message "Expected link" (thing : Thing.t)]
  ;;

  let comment_or_more_of_json json =
    let thing = Thing.of_json json in
    match thing with
    | Thing.(#comment | #more_comments) as thing -> thing
    | _ -> raise_s [%message "Expected comment or more_comments" (thing : Thing.t)]
  ;;

  let subreddit_of_json json =
    let thing = Thing.of_json json in
    match thing with
    | `Subreddit _ as thing -> thing
    | _ -> raise_s [%message "Expected subreddit" (thing : Thing.t)]
  ;;

  let get_listing child_of_json = handle_json_response (Listing.of_json child_of_json)
  let get_link_listing = get_listing link_of_json
  let get_subreddit_listing = get_listing subreddit_of_json
  let me = get ~endpoint:"/api/v1/me" ~params:[] return
  let karma = get ~endpoint:"/api/v1/me/karma" ~params:[] return
  let trophies = get ~endpoint:"/api/v1/me/trophies" ~params:[] return
  let needs_captcha = get ~endpoint:"/api/v1/me/needs_captcha" ~params:[] return

  let with_listing_params k ?pagination ?count ?limit ?show_all =
    let listing_params =
      let open Param_dsl in
      combine
        [ include_optional Pagination.params_of_t pagination
        ; optional' int "count" count
        ; optional' int "limit" limit
        ; optional' (const "all") "show" show_all
        ]
    in
    k ~listing_params
  ;;

  let prefs' which k ~listing_params ?subreddit_detail ?include_categories =
    let endpoint = sprintf "/api/%s" which in
    let params =
      let open Param_dsl in
      combine
        [ listing_params
        ; optional' bool "sr_detail" subreddit_detail
        ; optional' bool "include_categories" include_categories
        ]
    in
    post k ~endpoint ~params
  ;;

  let prefs endpoint k = with_listing_params (prefs' endpoint k)
  let friends = prefs "friends" return
  let blocked = prefs "blocked" return
  let messaging = prefs "messaging" return
  let trusted = prefs "trusted" return

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
    post
      ~endpoint
      ~params
      (handle_json_response (fun json ->
           let thing =
             Json.find json ~key:"json"
             |> Json.find ~key:"data"
             |> Json.find ~key:"things"
             |> Json.index ~index:0
             |> Thing.of_json
           in
           match thing with
           | `Comment _ as thing -> thing
           | _ -> raise_s [%message "Expected comment" (thing : Thing.t)]))
  ;;

  let delete ~fullname =
    let endpoint = "/api/comment" in
    let params =
      let open Param_dsl in
      Param_dsl.combine [ required' fullname_ "id" fullname ]
    in
    post ~endpoint ~params return
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
    post ~endpoint ~params return
  ;;

  let follow ~link ~follow =
    let endpoint = "/api/follow_post" in
    let params =
      let open Param_dsl in
      combine [ required' fullname_ "fullname" link; required' bool "follow" follow ]
    in
    post ~endpoint ~params return
  ;;

  let hide, unhide =
    let hide', unhide' = simple_toggle "hide" ignore_empty_object in
    (fun ~links -> hide' ~fullnames:links), fun ~links -> unhide' ~fullnames:links
  ;;

  let info ?subreddit query =
    let endpoint = optional_subreddit_endpoint ?subreddit "/api/info" in
    let params = Info_query.params_of_t query in
    get
      ~endpoint
      ~params
      (get_listing (fun json ->
           let thing = Thing.of_json json in
           match Thing.of_json json with
           | (`Link _ | `Comment _ | `Subreddit _) as thing -> thing
           | _ -> raise_s [%message "Unexpected kind in listing" (thing : Thing.t)]))
  ;;

  let lock, unlock = simple_toggle' "lock" ignore_empty_object
  let mark_nsfw, unmark_nsfw = simple_toggle' "marknsfw" ignore_empty_object

  let more_children
      ?id
      ?limit_children
      ~link:link_id
      ~children
      ~sort
      ?param_list_override
      connection
    =
    let endpoint = "/api/morechildren" in
    let link_fullname : Fullname.t = `Link link_id in
    let params =
      let open Param_dsl in
      combine
        [ api_type
        ; required Comment.Id36.to_string "children" children
        ; required' fullname_ "link_id" link_fullname
        ; optional' More_comments.Id36.to_string "id" id
        ; optional' bool "limit_children" limit_children
        ; required' Comment_sort.to_string "sort" sort
        ]
    in
    let sequencer = Connection.more_children_sequencer connection in
    Throttle.enqueue sequencer (fun () ->
        get return ~endpoint ~params ?param_list_override connection)
  ;;

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
    post ~endpoint ~params return
  ;;

  let report_award ~award_id =
    let endpoint = "/api/report_award" in
    let params = [ "award_id", [ award_id ] ] in
    post ~endpoint ~params return
  ;;

  let save ?category ~fullname =
    let endpoint = "/api/save" in
    let params =
      let open Param_dsl in
      combine [ required' fullname_ "id" fullname; optional' string "category" category ]
    in
    post ~endpoint ~params return
  ;;

  let unsave ~fullname =
    let endpoint = "/api/save" in
    let params = [ "id", [ Fullname.to_string fullname ] ] in
    post ~endpoint ~params return
  ;;

  let saved_categories = get ~endpoint:"/api/saved_categories" ~params:[] return

  let send_replies ~fullname ~enabled =
    let endpoint = "/api/sendreplies" in
    let params =
      let open Param_dsl in
      combine [ required' fullname_ "id" fullname; required' bool "state" enabled ]
    in
    post ~endpoint ~params return
  ;;

  let set_contest_mode ~fullname ~enabled =
    let endpoint = "/api/set_contest_mode" in
    let params =
      let open Param_dsl in
      combine
        [ api_type; required' fullname_ "id" fullname; required' bool "state" enabled ]
    in
    post ~endpoint ~params return
  ;;

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
    post ~endpoint ~params return
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
    post ~endpoint ~params return
  ;;

  let spoiler, unspoiler = simple_toggle' "spoiler" return

  let store_visits ~links =
    let endpoint = "/api/store_visits" in
    let params =
      let open Param_dsl in
      combine [ required Fullname.to_string "links" links ]
    in
    post ~endpoint ~params return
  ;;

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
        [ Link_kind.params_of_t kind
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
    post ~endpoint ~params return
  ;;

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
    post ~endpoint ~params return
  ;;

  let trending_subreddits = get ~endpoint:"/api/trending_subreddits" ~params:[] return

  let best' ~listing_params ?include_categories ?subreddit_detail =
    let endpoint = "/best" in
    let params =
      let open Param_dsl in
      combine
        [ listing_params
        ; optional' bool "include_categories" include_categories
        ; optional' bool "sr_detail" subreddit_detail
        ]
    in
    get ~endpoint ~params return
  ;;

  let best = with_listing_params best'

  let by_id ~fullnames =
    let endpoint =
      List.map fullnames ~f:Fullname.to_string
      |> String.concat ~sep:","
      |> sprintf "/by_id/%s"
    in
    get ~endpoint ~params:[] return
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
      ~link
    =
    let endpoint =
      optional_subreddit_endpoint ?subreddit (sprintf !"/comments/%{Link.Id36}" link)
    in
    let params =
      let open Param_dsl in
      combine
        [ optional' Comment.Id36.to_string "comment" comment
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
    get
      ~endpoint
      ~params
      (handle_json_response (fun json ->
           match Json.get_array json with
           | [ link_json; comment_forest_json ] ->
             let link =
               Listing.of_json link_of_json link_json |> Listing.children |> List.hd_exn
             in
             let comments = Listing.of_json comment_or_more_of_json comment_forest_json in
             link, comments
           | json -> raise_s [%message "Expected two-item response" (json : Json.t list)]))
  ;;

  let duplicates' ~listing_params ?crossposts_only ?subreddit_detail ?sort ~link =
    let endpoint = sprintf !"/duplicates/%{Link.Id36}" link in
    let params =
      let open Param_dsl in
      combine
        [ listing_params
        ; optional' bool "crossposts_only" crossposts_only
        ; optional' bool "sr_detail" subreddit_detail
        ; include_optional Duplicate_sort.params_of_t sort
        ]
    in
    get ~endpoint ~params get_link_listing
  ;;

  let duplicates = with_listing_params duplicates'

  let basic_post_listing'
      endpoint_part
      ~listing_params
      ?include_categories
      ?subreddit_detail
      ?subreddit
      ~extra_params
    =
    let endpoint = optional_subreddit_endpoint ?subreddit endpoint_part in
    let params =
      let open Param_dsl in
      combine
        [ listing_params
        ; optional' bool "include_categories" include_categories
        ; optional' bool "sr_detail" subreddit_detail
        ; extra_params
        ]
    in
    get ~endpoint ~params get_link_listing
  ;;

  let basic_post_listing endpoint =
    with_listing_params (basic_post_listing' endpoint ~extra_params:[])
  ;;

  let hot' ~listing_params ?location =
    let extra_params =
      let open Param_dsl in
      optional' string "location" location
    in
    basic_post_listing' "/hot" ~extra_params ~listing_params
  ;;

  let hot = with_listing_params hot'
  let new_ = basic_post_listing "/new"
  let rising = basic_post_listing "/rising"

  let top' ~listing_params ?since =
    let extra_params = Param_dsl.include_optional Historical_span.params_of_t since in
    basic_post_listing' "/top" ~extra_params ~listing_params
  ;;

  let top = with_listing_params top'

  let controversial' ~listing_params ?since =
    let extra_params = Param_dsl.include_optional Historical_span.params_of_t since in
    basic_post_listing' "/controversial" ~extra_params ~listing_params
  ;;

  let controversial = with_listing_params controversial'

  let random ?subreddit =
    let endpoint = optional_subreddit_endpoint ?subreddit "/random" in
    get ~endpoint ~params:[] return
  ;;

  let block = simple_post_fullname_as_id "block" return
  let collapse_message, uncollapse_message = simple_toggle "collapse_message" return

  let compose_message ?g_recaptcha_response ?from_subreddit ~to_ ~subject ~text =
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
    post ~endpoint ~params return
  ;;

  let delete_message = simple_post_fullname_as_id "del_msg" return
  let read_message, unread_message = simple_toggle "read_message" return
  let unblock_subreddit = simple_post_fullnames_as_id "unblock_subreddit" return

  let message_listing'
      endpoint
      ~listing_params
      ?include_categories
      ?mid
      ?subreddit_detail
      ~mark_read
    =
    let endpoint = "/message/" ^ endpoint in
    let params =
      let open Param_dsl in
      combine
        [ listing_params
        ; optional' bool "include_categories" include_categories
        ; optional' string "mid" mid
        ; optional' bool "sr_detail" subreddit_detail
        ; required bool "mark" mark_read
        ]
    in
    get ~endpoint ~params return
  ;;

  let message_listing endpoint = with_listing_params (message_listing' endpoint)
  let inbox = message_listing "inbox"
  let unread = message_listing "unread"
  let sent = message_listing "sent"

  let log' ~listing_params ?mod_filter ?subreddit_detail ?subreddit ?type_ =
    let endpoint = optional_subreddit_endpoint ?subreddit "/about/log" in
    let params =
      let open Param_dsl in
      combine
        [ listing_params
        ; include_optional Mod_filter.params_of_t mod_filter
        ; optional' bool "sr_detail" subreddit_detail
        ; optional' string "type" type_
        ]
    in
    get ~endpoint ~params return
  ;;

  let log = with_listing_params log'

  let mod_listing' ~listing_params ?location ?only ?subreddit ?subreddit_detail ~endpoint =
    let endpoint = optional_subreddit_endpoint ?subreddit endpoint in
    let params =
      let open Param_dsl in
      combine
        [ listing_params
        ; include_optional Links_or_comments.params_of_t only
        ; optional' bool "sr_detail" subreddit_detail
        ; optional' string "location" location
        ]
    in
    get ~endpoint ~params return
  ;;

  let mod_listing = with_listing_params mod_listing'
  let reports = mod_listing ~endpoint:"/reports"
  let spam = mod_listing ~endpoint:"/spam"
  let modqueue = mod_listing ~endpoint:"/modqueue"
  let unmoderated = mod_listing ~endpoint:"/unmoderated"
  let edited = mod_listing ~endpoint:"/edited"

  let accept_moderator_invite ~subreddit =
    let endpoint = sprintf !"/%{Subreddit_name}/api/accept_moderator_invite" subreddit in
    post ~endpoint ~params:api_type return
  ;;

  let approve = simple_post_fullname_as_id "approve" return
  let remove = simple_post_fullname_as_id "remove" return

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
    post
      ~endpoint
      ~params
      (handle_json_response (fun json ->
           let thing =
             Json.find json ~key:"json"
             |> Json.find ~key:"data"
             |> Json.find ~key:"things"
             |> Json.index ~index:0
             |> Thing.of_json
           in
           match thing with
           | (`Comment _ | `Link _) as thing -> thing
           | _ -> raise_s [%message "Expected comment or link" (thing : Thing.t)]))
  ;;

  let ignore_reports, unignore_reports = simple_toggle' "ignore_reports" return
  let leavecontributor = simple_post_fullname_as_id "leavecontributor" return
  let leavemoderator = simple_post_fullname_as_id "leavemoderator" return

  let mute_message_author, unmute_message_author =
    simple_toggle' "mute_message_author" return
  ;;

  let stylesheet ~subreddit =
    let endpoint = sprintf !"/r/%{Subreddit_name}/stylesheet" subreddit in
    get ~endpoint ~params:[] return
  ;;

  let search'
      ~listing_params
      ?category
      ?include_facets
      ?restrict_to_subreddit
      ?since
      ?sort
      ?subreddit_detail
      ?types
      ~query
    =
    let subreddit_part, restrict_param =
      match restrict_to_subreddit with
      | None -> "", []
      | Some subreddit ->
        sprintf !"/r/%{Subreddit_name}" subreddit, [ "restrict_sr", [ "true" ] ]
    in
    let endpoint = sprintf !"%s/search" subreddit_part in
    let params =
      let open Param_dsl in
      combine
        [ listing_params
        ; optional' string "category" category
        ; optional' bool "include_facets" include_facets
        ; required' string "q" query
        ; include_optional Historical_span.params_of_t since
        ; include_optional Search_sort.params_of_t sort
        ; optional' bool "sr_detail" subreddit_detail
        ; optional
            Search_type.to_string
            "type"
            (Option.map types ~f:Search_type.Set.to_list)
        ; restrict_param
        ]
    in
    get ~endpoint ~params return
  ;;

  let search = with_listing_params search'

  let about_endpoint'
      endpoint
      ~listing_params
      ?include_categories
      ?subreddit_detail
      ?user
      ~subreddit
    =
    let endpoint = sprintf !"/r/%{Subreddit_name}/about/%s" subreddit endpoint in
    let params =
      let open Param_dsl in
      combine
        [ listing_params
        ; optional' bool "include_categories" include_categories
        ; optional' bool "sr_detail" subreddit_detail
        ; optional' username_ "user" user
        ]
    in
    get ~endpoint ~params return
  ;;

  let about_endpoint endpoint = with_listing_params (about_endpoint' endpoint)
  let banned = about_endpoint "banned"
  let muted = about_endpoint "muted"
  let wiki_banned = about_endpoint "wikibanned"
  let contributors = about_endpoint "contributors"
  let wiki_contributors = about_endpoint "wikicontributors"
  let moderators = about_endpoint "moderators"

  let removal_endpoints ?(extra_params = []) ~subreddit endpoint =
    let endpoint = sprintf !"%{Subreddit_name}/api/%s" subreddit endpoint in
    post ~endpoint ~params:(Param_dsl.combine [ api_type; extra_params ])
  ;;

  let delete_subreddit_banner = removal_endpoints "delete_sr_banner" return
  let delete_subreddit_header = removal_endpoints "delete_sr_header" return
  let delete_subreddit_icon = removal_endpoints "delete_sr_icon" return

  let delete_subreddit_image ~image_name =
    let extra_params = Param_dsl.(required' string "img_name" image_name) in
    removal_endpoints "delete_sr_img" ~extra_params return
  ;;

  let recommended ?over_18 ~subreddits =
    let endpoint =
      List.map subreddits ~f:Subreddit_name.to_string
      |> String.concat ~sep:","
      |> sprintf "/api/recommend/sr/%s"
    in
    let params = Param_dsl.(optional' bool "over_18" over_18) in
    get ~endpoint ~params return
  ;;

  let search_subreddit_names ?exact ?include_over_18 ?include_unadvertisable ~query =
    let endpoint = "/api/search_reddit_names" in
    let params =
      let open Param_dsl in
      combine
        [ required' string "query" query
        ; optional' bool "exact" exact
        ; optional' bool "include_over_18" include_over_18
        ; optional' bool "include_unadvertisable" include_unadvertisable
        ]
    in
    get ~endpoint ~params return
  ;;

  let create_or_edit_subreddit
      ?comment_score_hide_mins
      ?wiki_edit_age
      ?wiki_edit_karma
      ~all_original_content
      ~allow_discovery
      ~allow_images
      ~allow_post_crossposts
      ~allow_top
      ~allow_videos
      ~api_type
      ~collapse_deleted_comments
      ~crowd_control_mode
      ~description
      ~disable_contributor_requests
      ~exclude_banned_modqueue
      ~free_form_reports
      ~g_recaptcha_response
      ~header_title
      ~hide_ads
      ~key_color
      ~lang
      ~link_type
      ~name
      ~original_content_tag_enabled
      ~over_18
      ~public_description
      ~restrict_commenting
      ~restrict_posting
      ~show_media
      ~show_media_preview
      ~spam_comments
      ~spam_links
      ~spam_selfposts
      ~spoilers_enabled
      ~subreddit
      ~submit_link_label
      ~submit_text
      ~submit_text_label
      ~suggested_comment_sort
      ~title
      ~type_
      ~wiki_mode
    =
    let endpoint = "/api/site_admin" in
    let params =
      let open Param_dsl in
      combine
        [ optional' int "comment_score_hide_mins" comment_score_hide_mins
        ; optional' int "wiki_edit_age" wiki_edit_age
        ; optional' int "wiki_edit_karma" wiki_edit_karma
        ; required' bool "all_original_content" all_original_content
        ; required' bool "allow_discovery" allow_discovery
        ; required' bool "allow_images" allow_images
        ; required' bool "allow_post_crossposts" allow_post_crossposts
        ; required' bool "allow_top" allow_top
        ; required' bool "allow_videos" allow_videos
        ; api_type
        ; required' bool "collapse_deleted_comments" collapse_deleted_comments
        ; required' bool "crowd_control_mode" crowd_control_mode
        ; required' string "description" description
        ; required' bool "disable_contributor_requests" disable_contributor_requests
        ; required' bool "exclude_banned_modqueue" exclude_banned_modqueue
        ; required' bool "free_form_reports" free_form_reports
        ; optional' string "g_recaptcha_response" g_recaptcha_response
        ; required' string "header_title" header_title
        ; required' bool "hide_ads" hide_ads
        ; required' string "key_color" key_color
        ; required' string "lang" lang
        ; Link_type.params_of_t link_type
        ; required' string "name" name
        ; required' bool "original_content_tag_enabled" original_content_tag_enabled
        ; required' bool "over_18" over_18
        ; required' string "public_description" public_description
        ; required' bool "restrict_commenting" restrict_commenting
        ; required' bool "restrict_posting" restrict_posting
        ; required' bool "show_media" show_media
        ; required' bool "show_media_preview" show_media_preview
        ; required' Spam_level.to_string "spam_comments" spam_comments
        ; required' Spam_level.to_string "spam_links" spam_links
        ; required' Spam_level.to_string "spam_selfposts" spam_selfposts
        ; required' bool "spoilers_enabled" spoilers_enabled
        ; required' Subreddit_name.to_string "sr" subreddit
        ; required' string "submit_link_label" submit_link_label
        ; required' string "submit_text" submit_text
        ; required' string "submit_text_label" submit_text_label
        ; required' Comment_sort.to_string "suggested_comment_sort" suggested_comment_sort
        ; required' string "title" title
        ; required' Subreddit_type.to_string "type_" type_
        ; required' Wiki_mode.to_string "wikimode" wiki_mode
        ]
    in
    post ~endpoint ~params return
  ;;

  let submit_text ~subreddit =
    let endpoint = sprintf !"/r/%{Subreddit_name}/api/submit_text" subreddit in
    get ~endpoint ~params:[] return
  ;;

  let subreddit_autocomplete ?include_over_18 ?include_profiles ~query =
    let endpoint = "/api/subreddit_autocomplete" in
    let params =
      let open Param_dsl in
      combine
        [ optional' bool "include_over_18" include_over_18
        ; optional' bool "include_profiles" include_profiles
        ; required' string "query" query
        ]
    in
    get ~endpoint ~params return
  ;;

  let subreddit_autocomplete_v2
      ?limit
      ?include_categories
      ?include_over_18
      ?include_profiles
      ~query
    =
    let endpoint = "/api/subreddit_autocomplete_v2" in
    let params =
      let open Param_dsl in
      combine
        [ optional' int "limit" limit
        ; optional' bool "include_categories" include_categories
        ; optional' bool "include_over_18" include_over_18
        ; optional' bool "include_profiles" include_profiles
        ; required' string "query" query
        ]
    in
    get ~endpoint ~params return
  ;;

  let subreddit_stylesheet ?reason ~operation ~stylesheet_contents ~subreddit =
    let endpoint = sprintf !"/r/%{Subreddit_name}/api/subreddit_stylesheet" subreddit in
    let params =
      let open Param_dsl in
      combine
        [ api_type
        ; optional' string "reason" reason
        ; required' Stylesheet_operation.to_string "op" operation
        ; required' string "stylesheet_contents" stylesheet_contents
        ]
    in
    post ~endpoint ~params return
  ;;

  let subscribe ?skip_initial_defaults ~action =
    let endpoint = "/api/subscribe" in
    let params =
      let open Param_dsl in
      combine
        [ required' Subscription_action.to_string "action" action
        ; optional' bool "skip_initial_defaults" skip_initial_defaults
        ]
    in
    post ~endpoint ~params return
  ;;

  let upload_sr_img ?form_id ~file ~header ~image_type ~name ~subreddit ~upload_type =
    let endpoint = sprintf !"/r/%{Subreddit_name}/api/upload_sr_img" subreddit in
    let params =
      let header = Bool.to_int header in
      let open Param_dsl in
      combine
        [ optional' string "formid" form_id
        ; required' string "file" file
        ; required' int "header" header
        ; required' Image_type.to_string "img_type" image_type
        ; required' string "name" name
        ; required' Upload_type.to_string "upload_type" upload_type
        ]
    in
    post ~endpoint ~params return
  ;;

  let search_profiles' ~listing_params ?subreddit_detail ?sort ~query =
    let endpoint = "/profiles/search" in
    let params =
      let open Param_dsl in
      combine
        [ listing_params
        ; optional' bool "sr_detail" subreddit_detail
        ; required' string "q" query
        ; optional' Subreddit_search_sort.to_string "sort" sort
        ]
    in
    get ~endpoint ~params return
  ;;

  let search_profiles = with_listing_params search_profiles'

  let about ~subreddit =
    let endpoint = sprintf !"/r/%{Subreddit_name}/about" subreddit in
    get ~endpoint ~params:[] return
  ;;

  let subreddit_about ?(params = []) ~subreddit endpoint =
    let endpoint = sprintf !"/r/%{Subreddit_name}/about/%s" subreddit endpoint in
    get ~endpoint ~params
  ;;

  let subreddit_settings ?created ?location =
    let params =
      let open Param_dsl in
      combine [ optional' bool "created" created; optional' string "location" location ]
    in
    subreddit_about ~params "edit" return
  ;;

  let subreddit_rules = subreddit_about "rules" return
  let subreddit_traffic = subreddit_about "traffic" return
  let subreddit_sidebar = subreddit_about "sidebar" return

  let sticky ?number ~subreddit =
    let endpoint = sprintf !"/r/%{Subreddit_name}/sticky" subreddit in
    let params =
      let open Param_dsl in
      combine [ optional' int "num" number ]
    in
    get ~endpoint ~params return
  ;;

  let get_subreddits' ~listing_params ?include_categories ?subreddit_detail ~relationship =
    let endpoint = sprintf !"/subreddits/mine/%{Subreddit_relationship}" relationship in
    let params =
      let open Param_dsl in
      combine
        [ listing_params
        ; optional' string "sr_detail" subreddit_detail
        ; optional' bool "include_categories" include_categories
        ]
    in
    get ~endpoint ~params return
  ;;

  let get_subreddits = with_listing_params get_subreddits'

  let search_subreddits' ~listing_params ?show_users ?sort ?subreddit_detail ~query =
    let endpoint = "/subreddits/search" in
    let params =
      let open Param_dsl in
      combine
        [ listing_params
        ; optional' bool "sr_detail" subreddit_detail
        ; optional' bool "show_users" show_users
        ; required' string "q" query
        ; optional' Subreddit_search_sort.to_string "sort" sort
        ]
    in
    get ~endpoint ~params return
  ;;

  let search_subreddits = with_listing_params search_subreddits'

  let list_subreddits'
      ~listing_params
      ?subreddit_detail
      ?include_categories
      ?show_users
      ~sort
    =
    let endpoint = sprintf !"/subreddits/%{Subreddit_listing_sort}" sort in
    let params =
      let open Param_dsl in
      combine
        [ listing_params
        ; optional' bool "sr_detail" subreddit_detail
        ; optional' bool "include_categories" include_categories
        ; optional' bool "show_users" show_users
        ]
    in
    get ~endpoint ~params return
  ;;

  let list_subreddits = with_listing_params list_subreddits'

  let list_user_subreddits' ~listing_params ?subreddit_detail ?include_categories ~sort =
    let endpoint = sprintf !"/users/%{User_subreddit_sort}" sort in
    let params =
      let open Param_dsl in
      combine
        [ listing_params
        ; optional' bool "sr_detail" subreddit_detail
        ; optional' bool "include_categories" include_categories
        ]
    in
    get ~endpoint ~params get_subreddit_listing
  ;;

  let list_user_subreddits = with_listing_params list_user_subreddits'

  let add_relationship
      ~relationship
      ~username
      ~duration
      ?subreddit
      ?note
      ?ban_reason
      ?ban_message
      ?ban_context
    =
    let endpoint = optional_subreddit_endpoint ?subreddit "/api/friend" in
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
    post ~endpoint ~params ignore_success_response
  ;;

  let remove_relationship ~relationship ~username ?subreddit =
    let endpoint = optional_subreddit_endpoint ?subreddit "/api/unfriend" in
    let params =
      let open Param_dsl in
      combine
        [ Relationship.params_of_t relationship
        ; api_type
        ; required' username_ "name" username
        ]
    in
    post ~endpoint ~params ignore_success_response
  ;;

  let add_or_remove_wiki_editor
      ~add_or_remove
      ~page:({ subreddit; page } : Wiki_page.Id.t)
      ~user
    =
    let endpoint = optional_subreddit_endpoint ?subreddit "/api/wiki/alloweditor/act" in
    let params =
      let open Param_dsl in
      combine
        [ required' Add_or_remove.to_string "act" add_or_remove
        ; required' string "page" page
        ; required' username_ "username" user
        ]
    in
    post ~endpoint ~params return
  ;;

  let edit_wiki_page
      ?previous
      ?reason
      ~content
      ~page:({ subreddit; page } : Wiki_page.Id.t)
    =
    let endpoint = optional_subreddit_endpoint ?subreddit "/api/wiki/edit" in
    let params =
      let open Param_dsl in
      combine
        [ required' string "content" content
        ; required' string "page" page
        ; optional' Uuid.to_string "previous" previous
        ; optional' string "reason" reason
        ]
    in
    post ~endpoint ~params (fun (response, body) ->
        let%bind json = Cohttp_async.Body.to_string body |> some >>| Json.of_string in
        match Cohttp.Response.status response, json with
        | #Cohttp.Code.success_status, `Object [] -> return (Ok ())
        | `Conflict, json -> return (Error (Wiki_page.Edit_conflict.of_json json))
        | _, _ -> none)
  ;;

  let toggle_wiki_revision_visibility
      ~page:({ subreddit; page } : Wiki_page.Id.t)
      ~revision
    =
    let endpoint = optional_subreddit_endpoint ?subreddit "/api/wiki/hide" in
    let params =
      let open Param_dsl in
      combine [ required' string "page" page; required' string "revision" revision ]
    in
    post ~endpoint ~params return
  ;;

  let revert_wiki_page ~page:({ subreddit; page } : Wiki_page.Id.t) ~revision =
    let endpoint = optional_subreddit_endpoint ?subreddit "/api/wiki/revert" in
    let params =
      let open Param_dsl in
      combine [ required' string "page" page; required' string "revision" revision ]
    in
    post ~endpoint ~params return
  ;;

  let wiki_discussions'
      ~listing_params
      ?subreddit_detail
      ~page:({ subreddit; page } : Wiki_page.Id.t)
    =
    let endpoint =
      optional_subreddit_endpoint ?subreddit (sprintf "/wiki/discussions/%s" page)
    in
    let params =
      let open Param_dsl in
      combine [ listing_params; optional' string "sr_detail" subreddit_detail ]
    in
    get ~endpoint ~params return
  ;;

  let wiki_discussions = with_listing_params wiki_discussions'

  let wiki_pages ?subreddit =
    let endpoint = optional_subreddit_endpoint ?subreddit "/wiki/pages" in
    get ~endpoint ~params:[] return
  ;;

  let subreddit_wiki_revisions' ~listing_params ?subreddit_detail ?subreddit =
    let endpoint = optional_subreddit_endpoint ?subreddit "/wiki/revisions" in
    let params =
      let open Param_dsl in
      combine [ listing_params; optional' string "sr_detail" subreddit_detail ]
    in
    get ~endpoint ~params return
  ;;

  let subreddit_wiki_revisions = with_listing_params subreddit_wiki_revisions'

  let wiki_page_revisions'
      ~listing_params
      ?subreddit_detail
      ~page:({ subreddit; page } : Wiki_page.Id.t)
    =
    let endpoint =
      optional_subreddit_endpoint ?subreddit (sprintf "/wiki/revisions/%s" page)
    in
    let params =
      let open Param_dsl in
      combine [ listing_params; optional' string "sr_detail" subreddit_detail ]
    in
    get ~endpoint ~params return
  ;;

  let wiki_page_revisions = with_listing_params wiki_page_revisions'

  let wiki_permissions ~page:({ subreddit; page } : Wiki_page.Id.t) =
    let endpoint =
      optional_subreddit_endpoint ?subreddit (sprintf "/wiki/settings/%s" page)
    in
    get ~endpoint ~params:[] return
  ;;

  let set_wiki_permissions
      ~listed
      ~page:({ subreddit; page } : Wiki_page.Id.t)
      ~permission_level
    =
    let endpoint =
      optional_subreddit_endpoint ?subreddit (sprintf "/wiki/settings/%s" page)
    in
    let params =
      let open Param_dsl in
      combine
        [ required' bool "listed" listed
        ; required' string "page" page
        ; required' int "permlevel" permission_level
        ]
    in
    post ~endpoint ~params return
  ;;

  let wiki_page ?compare_revisions ~page:({ subreddit; page } : Wiki_page.Id.t) =
    let endpoint = optional_subreddit_endpoint ?subreddit (sprintf "/wiki/%s" page) in
    let v1, v2 = Option.value compare_revisions ~default:(None, None) in
    let params =
      let open Param_dsl in
      combine
        [ required' string "page" page
        ; optional' string "v" v1
        ; optional' string "v2" v2
        ]
    in
    get ~endpoint ~params (handle_json_response Wiki_page.of_json)
  ;;
end

module Raw_param = struct
  type _ t = Cohttp.Response.t * Cohttp_async.Body.t

  let finalize
      (_ : Cohttp.Response.t * Cohttp_async.Body.t -> 'a Deferred.Option.t)
      response
    =
    return response
  ;;
end

module Raw = Make (Connection) (Raw_param)

module Exn_param = struct
  type 'a t = 'a

  let finalize f response =
    match%bind f response with
    | Some x -> return x
    | None ->
      let response, body = response in
      raise_s
        [%message
          "HTTP error" (response : Cohttp.Response.t) (body : Cohttp_async.Body.t)]
  ;;
end

module Exn = Make (Connection) (Exn_param)

module Typed_param = struct
  type 'a t = ('a, Cohttp.Response.t * Cohttp_async.Body.t) Result.t

  let finalize f response = f response >>| Result.of_option ~error:response
end

include Make (Connection) (Typed_param)

module For_testing =
  Make
    (struct
      type t = Cohttp.Response.t * Cohttp_async.Body.t

      let get t _ = return t
      let post_form t _ ~params:_ = return t
      let more_children_sequencer _ = Sequencer.create ()
    end)
    (Typed_param)
