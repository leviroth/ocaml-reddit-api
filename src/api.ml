open! Core
open! Async

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
      [ Optional' ("comment", comment >>| Fullname.Comment_id.to_string)
      ; Optional' ("context", context >>| Int.to_string)
      ; Optional' ("depth", depth >>| Int.to_string)
      ; Optional' ("limit", limit >>| Int.to_string)
      ; Optional' ("showedits", showedits >>| Bool.to_string)
      ; Optional' ("showmore", showmore >>| Bool.to_string)
      ; Optional' ("sort", sort >>| Comment_sort.to_string)
      ; Optional' ("sr_detail", sr_detail >>| Bool.to_string)
      ; Optional' ("threaded", threaded >>| Bool.to_string)
      ; Optional' ("truncate", truncate >>| Int.to_string)
      ]
  in
  let uri = Uri.add_query_params base_uri params in
  let%bind _response, body = Connection.get connection uri in
  Cohttp_async.Body.to_string body
  >>| Yojson.Safe.from_string
  >>| Yojson.Safe.Util.to_list
  >>| List.map ~f:Thing.of_json
;;
