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
  type t =
    | Required of string list
    | Required' of string
    | Optional of string list option
    | Optional' of string option

  let _ = Optional None

  let make field_names_and_ts =
    List.filter_map field_names_and_ts ~f:(function
        | name, (Required values | Optional (Some values)) -> Some (name, values)
        | name, (Required' value | Optional' (Some value)) -> Some (name, [ value ])
        | _, (Optional None | Optional' None) -> None)
  ;;
end

let api_type : string * Param_dsl.t = "api_type", Required' "json"

let comment ?return_rtjson ?richtext_json ~parent ~text =
  let endpoint = "/api/comment" in
  let params =
    let open Option.Monad_infix in
    Param_dsl.make
      [ api_type
      ; "thing_id", Required' (Fullname.to_string parent)
      ; "text", Required' text
      ; "return_rtjson", Optional' (return_rtjson >>| Bool.to_string)
      ; "richtext_json", Optional' (richtext_json >>| Yojson.Safe.to_string)
      ]
  in
  post ~endpoint ~params
;;

let delete ~fullname =
  let endpoint = "/api/comment" in
  let params = Param_dsl.make [ "id", Required' (Fullname.to_string fullname) ] in
  post ~endpoint ~params
;;

let edit ?return_rtjson ?richtext_json ~fullname ~text =
  let endpoint = "/api/editusertext" in
  let params =
    let open Option.Monad_infix in
    Param_dsl.make
      [ api_type
      ; "thing_id", Required' (Fullname.to_string fullname)
      ; "text", Required' text
      ; "return_rtjson", Optional' (return_rtjson >>| Bool.to_string)
      ; "richtext_json", Optional' (richtext_json >>| Yojson.Safe.to_string)
      ]
  in
  post ~endpoint ~params
;;

let follow ~submission ~follow =
  let endpoint = "/api/follow_post" in
  let params =
    Param_dsl.make
      [ "fullname", Required' (Fullname.to_string submission)
      ; "follow", Required' (Bool.to_string follow)
      ]
  in
  post ~endpoint ~params
;;

let simple_toggle verb =
  let toggle_one_direction ~verb ~fullnames =
    let endpoint = sprintf "/api/%s" verb in
    let params =
      Param_dsl.make [ "id", Required (List.map fullnames ~f:Fullname.to_string) ]
    in
    post ~endpoint ~params
  in
  toggle_one_direction ~verb, toggle_one_direction ~verb:("un" ^ verb)
;;

let simple_toggle' verb =
  let toggle_one_direction ~verb ~fullname =
    let endpoint = sprintf "/api/%s" verb in
    let params = Param_dsl.make [ "id", Required' (Fullname.to_string fullname) ] in
    post ~endpoint ~params
  in
  toggle_one_direction ~verb, toggle_one_direction ~verb:("un" ^ verb)
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
    | Qa
    | Live
  [@@deriving sexp]

  let to_string t = sexp_of_t t |> Sexp.to_string |> String.lowercase
end

(* TODO: mutex *)
let more_children ?id ?limit_children ~submission ~children ~sort =
  let endpoint = "/api/morechildren" in
  let params =
    let open Option.Monad_infix in
    Param_dsl.make
      [ api_type
      ; "children", Required (List.map children ~f:Fullname.Comment_id.to_string)
      ; "link_id", Required' (Fullname.to_string submission)
      ; "id", Optional' (id >>| Fullname.More_children_id.to_string)
      ; "limit_children", Optional' (limit_children >>| Bool.to_string)
      ; "sort", Required' (Comment_sort.to_string sort)
      ]
  in
  get ~endpoint ~params
;;

module Report_target = struct
  type t =
    | Modmail_conversation of Fullname.Modmail_conversation_id.t
    | Fullname of Fullname.t
  [@@deriving sexp]

  let param_of_t t =
    match t with
    | Modmail_conversation id ->
      "modmail_conv_id", [ Fullname.Modmail_conversation_id.to_string id ]
    | Fullname fullname -> "thing_id", [ Fullname.to_string fullname ]
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
    Report_target.param_of_t target
    ::
    (let open Option.Monad_infix in
    Param_dsl.make
      [ api_type
      ; "reason", Required' reason
      ; "additional_info", Optional' additional_info
      ; "custom_text", Optional' custom_text
      ; "other_reason", Optional' other_reason
      ; "rule_reason", Optional' rule_reason
      ; "site_reason", Optional' site_reason
      ; "sr_name", Optional' sr_name
      ; "from_modmail", Optional' (from_modmail >>| Bool.to_string)
      ; "from_help_desk", Optional' (from_help_desk >>| Bool.to_string)
      ])
  in
  post ~endpoint ~params
;;

let report_award ~award_id =
  let endpoint = "/api/report_award" in
  let params = Param_dsl.make [ "award_id", Required' award_id ] in
  post ~endpoint ~params
;;

let save ?category ~fullname =
  let endpoint = "/api/save" in
  let params =
    Param_dsl.make
      [ "id", Required' (Fullname.to_string fullname); "category", Optional' category ]
  in
  post ~endpoint ~params
;;

let unsave ~fullname =
  let endpoint = "/api/save" in
  let params = Param_dsl.make [ "id", Required' (Fullname.to_string fullname) ] in
  post ~endpoint ~params
;;

let saved_categories = get ~endpoint:"/api/saved_categories" ~params:[]

let send_replies ~fullname ~enabled =
  let endpoint = "/api/sendreplies" in
  let params =
    Param_dsl.make
      [ "id", Required' (Fullname.to_string fullname); "state", Required' enabled ]
  in
  post ~endpoint ~params
;;

let set_contest_mode ~fullname ~enabled =
  let endpoint = "/api/set_contest_mode" in
  let params =
    Param_dsl.make
      [ api_type
      ; "id", Required' (Fullname.to_string fullname)
      ; "state", Required' enabled
      ]
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
  let open Option.Monad_infix in
  let params =
    Sticky_state.params_of_t sticky_state
    @ Param_dsl.make
        [ api_type
        ; "id", Required' (Fullname.to_string fullname)
        ; "to_profile", Optional' (to_profile >>| Bool.to_string)
        ]
  in
  post ~endpoint ~params
;;

let set_suggested_sort ~fullname ~sort =
  let endpoint = "/api/set_suggested_sort" in
  let params =
    Param_dsl.make
      [ api_type
      ; "id", Required' (Fullname.to_string fullname)
      ; ( "sort"
        , Required' (Option.value_map sort ~f:Comment_sort.to_string ~default:"blank") )
      ]
  in
  post ~endpoint ~params
;;

let spoiler, unspoiler = simple_toggle' "spoiler"

let store_visits ~submissions =
  let endpoint = "/api/store_visits" in
  let params =
    Param_dsl.make [ "links", Required (List.map submissions ~f:Fullname.to_string) ]
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

(* TODO other parameters? *)
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
    Submission_kind.params_of_t kind
    @
    let open Option.Monad_infix in
    Param_dsl.make
      [ api_type
      ; "sr", Required' (Subreddit_name.to_string subreddit)
      ; "title", Required' title
      ; "ad", Optional' (ad >>| Bool.to_string)
      ; "nsfw", Optional' (nsfw >>| Bool.to_string)
      ; "resubmit", Optional' (resubmit >>| Bool.to_string)
      ; "sendreplies", Optional' (sendreplies >>| Bool.to_string)
      ; "spoiler", Optional' (spoiler >>| Bool.to_string)
        (* TODO Do these have to go together? *)
      ; "flair_id", Optional' flair_id
      ; "flair_text", Optional' flair_text
      ; "collection_id", Optional' collection_id
      ; ( "event_start"
        , Optional' (event_start >>| Time.to_string_iso8601_basic ~zone:Time.Zone.utc) )
      ; ( "event_end"
        , Optional' (event_end >>| Time.to_string_iso8601_basic ~zone:Time.Zone.utc) )
      ; "event_tz", Optional' event_tz
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

  let param_of_t t = "dir", [ int_of_t t |> Int.to_string ]
end

let vote ?rank ~direction ~fullname =
  let endpoint = "/api/vote" in
  let params =
    Vote_direction.param_of_t direction
    ::
    (let open Option.Monad_infix in
    Param_dsl.make
      [ "fullname", Required' (Fullname.to_string fullname)
      ; "rank", Optional' (rank >>| Int.to_string)
      ])
  in
  post ~endpoint ~params
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
    ?sr_detail
    ?threaded
    ?truncate
    ~submission
  =
  let endpoint =
    let subreddit_part =
      Option.value_map subreddit ~default:"" ~f:(fun name ->
          sprintf !"/r/%{Subreddit_name}" name)
    in
    sprintf !"%s/comments/%{Fullname.Submission_id}" subreddit_part submission
  in
  let params =
    let open Option.Monad_infix in
    Param_dsl.make
      [ "comment", Optional' (comment >>| Fullname.Comment_id.to_string)
      ; "context", Optional' (context >>| Int.to_string)
      ; "depth", Optional' (depth >>| Int.to_string)
      ; "limit", Optional' (limit >>| Int.to_string)
      ; "showedits", Optional' (showedits >>| Bool.to_string)
      ; "showmore", Optional' (showmore >>| Bool.to_string)
      ; "sort", Optional' (sort >>| Comment_sort.to_string)
      ; "sr_detail", Optional' (sr_detail >>| Bool.to_string)
      ; "threaded", Optional' (threaded >>| Bool.to_string)
      ; "truncate", Optional' (truncate >>| Int.to_string)
      ]
  in
  get ~endpoint ~params
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
    let open Option.Let_syntax in
    Param_dsl.make
      [ api_type
      ; "name", Required' (Username.to_string username)
      ; "note", Optional' note
      ; "ban_reason", Optional' ban_reason
      ; "ban_message", Optional' ban_message
      ; "ban_context", Optional' (ban_context >>| Fullname.to_string)
      ]
  in
  post ~endpoint ~params
;;

let remove_relationship ~relationship ~username ~subreddit =
  let endpoint = sprintf !"/r/%{Subreddit_name}/api/unfriend" subreddit in
  let params =
    Relationship.params_of_t relationship
    @ Param_dsl.make [ api_type; "name", Required' (Username.to_string username) ]
  in
  post ~endpoint ~params
;;
