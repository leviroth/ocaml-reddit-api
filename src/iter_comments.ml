open! Core
open! Async
open Thing

let cohttp_error_sexp (response, body) =
  let%bind body = Cohttp_async.Body.to_string body in
  return [%sexp { response : Cohttp.Response.t; body : string }]
;;

let retry_or_log_unexpected retry_manager here f =
  match%bind Retry_manager.call retry_manager f with
  | Ok v -> return (Some v)
  | Error error ->
    let%bind error = cohttp_error_sexp error in
    Log.Global.error_s
      [%message
        "Unexpected response from Reddit" (here : Source_code_position.t) (error : Sexp.t)];
    return None
;;

let iter_item_if_comment item ~f =
  match item with
  | `More_comments _ -> return ()
  | `Comment comment -> f comment
;;

let children item ~connection ~retry_manager ~link =
  match item with
  | `Comment comment -> return (Comment.replies comment)
  | `More_comments more_comments ->
    (match More_comments.details more_comments with
    | By_children more_comments ->
      retry_or_log_unexpected retry_manager [%here] (fun () ->
          Api.more_children ~link ~more_comments ~sort:New connection)
    | By_parent parent ->
      retry_or_log_unexpected retry_manager [%here] (fun () ->
          Api.comments ~comment:parent connection ~link)
      >>| Option.map
            ~f:(fun ({ comment_forest; link = _ } as response : Comment_response.t) ->
              match comment_forest with
              | [ `Comment comment ] -> Comment.replies comment
              | _ ->
                raise_s
                  [%message
                    "Expected single comment at root of response"
                      (link : Link.Id.t)
                      (parent : Comment.Id.t)
                      (response : Comment_response.t)]))
    >>| Option.value ~default:[]
;;

let iter_comments
    connection
    ~retry_manager
    ~comment_response:({ link; comment_forest } : Comment_response.t)
    ~f
  =
  let link = Link.id link in
  let queue = Queue.of_list comment_forest in
  Deferred.repeat_until_finished () (fun () ->
      match Queue.dequeue queue with
      | None -> return (`Finished ())
      | Some item ->
        let%bind () = iter_item_if_comment item ~f in
        let%bind children = children item ~connection ~retry_manager ~link in
        Queue.enqueue_all queue children;
        return (`Repeat ()))
;;
