open! Core
open! Async
open! Import
open Reddit_api_kernel
open Thing

let retry_or_log_unexpected retry_manager here endpoint =
  match%bind Retry_manager.call retry_manager endpoint with
  | Ok v -> return (Some v)
  | Error error ->
    [%log.error
      log
        "Unexpected response from Reddit"
        (here : Source_code_position.t)
        (error : Retry_manager.Permanent_error.t)];
    return None
;;

let children item ~retry_manager ~link =
  match item with
  | `Comment comment -> return (Comment.replies comment)
  | `More_comments more_comments ->
    (match More_comments.details more_comments with
    | By_children more_comments ->
      retry_or_log_unexpected
        retry_manager
        [%here]
        (Endpoint.more_children ~link ~more_comments ~sort:New ())
    | By_parent parent ->
      retry_or_log_unexpected
        retry_manager
        [%here]
        (Endpoint.comments ~comment:parent () ~link)
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
    retry_manager
    ~comment_response:({ link; comment_forest } : Comment_response.t)
  =
  let link = Link.id link in
  let queue = Queue.of_list comment_forest in
  Pipe.create_reader ~close_on_exception:false (fun writer ->
      Deferred.repeat_until_finished () (fun () ->
          match Pipe.is_closed writer with
          | true -> return (`Finished ())
          | false ->
            (match Queue.dequeue queue with
            | None -> return (`Finished ())
            | Some item ->
              let%bind () =
                match item with
                | `More_comments _ -> return ()
                | `Comment comment -> Pipe.write_if_open writer comment
              in
              let%bind children = children item ~retry_manager ~link in
              Queue.enqueue_all queue children;
              return (`Repeat ()))))
;;
