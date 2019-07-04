open! Core
open! Async

let comment ?return_rtjson ?richtext_json connection ~parent ~text =
  let uri = Uri.of_string "https://oauth.reddit.com/api/comment" in
  let params =
    let open Option.Monad_infix in
    Param_dsl.make
      [ "api_type", Required' "json"
      ; "thing_id", Required' (Fullname.to_string parent)
      ; "text", Required' text
      ; "return_rtjson", Optional' (return_rtjson >>| Bool.to_string)
      ; "richtext_json", Optional' (richtext_json >>| Yojson.Safe.to_string)
      ]
  in
  Connection.post_form connection uri ~params
;;

let delete connection ~fullname =
  let uri = Uri.of_string "https://oauth.reddit.com/api/comment" in
  let params = Param_dsl.make [ "id", Required' (Fullname.to_string fullname) ] in
  Connection.post_form connection uri ~params
;;

let edit ?return_rtjson ?richtext_json connection ~fullname ~text =
  let uri = Uri.of_string "https://oauth.reddit.com/api/editusertext" in
  let params =
    let open Option.Monad_infix in
    Param_dsl.make
      [ "api_type", Required' "json"
      ; "thing_id", Required' (Fullname.to_string fullname)
      ; "text", Required' text
      ; "return_rtjson", Optional' (return_rtjson >>| Bool.to_string)
      ; "richtext_json", Optional' (richtext_json >>| Yojson.Safe.to_string)
      ]
  in
  Connection.post_form connection uri ~params
;;

let follow connection ~submission ~follow =
  let uri = Uri.of_string "https://oauth.reddit.com/api/follow_post" in
  let params =
    Param_dsl.make
      [ "fullname", Required' (Fullname.to_string submission)
      ; "follow", Required' (Bool.to_string follow)
      ]
  in
  Connection.post_form connection uri ~params
;;

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

module Info_query = struct
  type t =
    | Id of Fullname.t list
    | Url of Uri.t
end

let info ?subreddit connection (query : Info_query.t) =
  let base_uri =
    let subreddit_part =
      Option.value_map subreddit ~default:"" ~f:(sprintf !"/r/%{Subreddit_name}")
    in
    sprintf !"https://oauth.reddit.com%s/api/info" subreddit_part |> Uri.of_string
  in
  let params =
    match query with
    | Id fullnames -> "id", List.map fullnames ~f:Fullname.to_string
    | Url uri -> "url", [ Uri.to_string uri ]
  in
  let uri = Uri.add_query_param base_uri params in
  let%bind _response, body = Connection.get connection uri in
  Cohttp_async.Body.to_string body
  >>| Yojson.Safe.from_string
  >>| Yojson.Safe.Util.to_list
  >>| List.map ~f:Thing.of_json
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
    connection
    ~submission
  =
  let base_uri =
    let subreddit_part =
      Option.value_map subreddit ~default:"" ~f:(fun name ->
          sprintf !"/r/%{Subreddit_name}" name)
    in
    sprintf
      !"https://oauth.reddit.com%scomments/%{Fullname.Submission_id}"
      subreddit_part
      submission
    |> Uri.of_string
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
  let uri = Uri.add_query_params base_uri params in
  let%bind _response, body = Connection.get connection uri in
  Cohttp_async.Body.to_string body
  >>| Yojson.Safe.from_string
  >>| Yojson.Safe.Util.to_list
  >>| List.map ~f:Thing.of_json
;;
