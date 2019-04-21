open! Core
open! Async

let add_user_agent l =
  ("User-Agent", "OCaml Api Wrapper/0.1 - developed by /u/L72_Elite_kraken") :: l
;;

module Auth = struct
  module Config = struct
    type t =
      { client_id : string
      ; client_secret : string
      ; password : string
      ; username : string
      }
    [@@deriving sexp]

    let basic_auth_string t =
      Cohttp.Auth.string_of_credential (`Basic (t.client_id, t.client_secret))
    ;;
  end

  module Access_token = struct
    (* TODO: [with_t] function? *)
    type t =
      { token : string
      ; expiration : Time.t
      }
    [@@deriving sexp]

    let is_almost_expired { expiration; _ } =
      let time_with_padding = Time.add (Time.now ()) (Time.Span.of_int_sec 10) in
      Time.( <= ) expiration time_with_padding
    ;;
  end

  module Rate_limiter : sig
    type t [@@deriving sexp_of]

    val create : unit -> t

    val with_t
      :  t
      -> f:(unit -> (Cohttp.Response.t * Cohttp.Body.t) Deferred.t)
      -> (Cohttp.Response.t * Cohttp.Body.t) Deferred.t
  end = struct
    let tolerance = Time.Span.of_int_sec 10

    module Remaining_call_state = struct
      type t =
        | Known of int
        | Waiting_for_refresh
      [@@deriving sexp_of]
    end

    type t =
      { mutable remaining_api_calls : Remaining_call_state.t
      ; mutable reset_wait : Time.Span.t
      ; jobs : (unit -> unit) Queue.t
      }
    [@@deriving sexp_of]

    let create () =
      { remaining_api_calls = Known 600
      ; reset_wait = Time.Span.of_int_sec 600
      ; jobs = Queue.create ()
      }
    ;;

    let run_job_from_queue t =
      let job = Queue.dequeue_exn t.jobs in
      job ()
    ;;

    let rec clear_queue t =
      match Queue.is_empty t.jobs with
      | true -> ()
      | false ->
        (match t.remaining_api_calls with
        | Waiting_for_refresh -> ()
        | Known 0 ->
          upon (after t.reset_wait) (fun () -> run_job_from_queue t);
          clear_queue t
        | Known n ->
          t.remaining_api_calls <- Known (n - 1);
          run_job_from_queue t;
          clear_queue t)
    ;;

    (* TODO: This is a bad infinite loop *)

    let with_t t ~f =
      let ivar = Ivar.create () in
      Queue.enqueue t.jobs (fun () ->
          upon (f ()) (fun ((response, _body) as result) ->
              let headers = Cohttp.Response.headers response in
              Cohttp.Header.get headers "X-Ratelimit-Remaining"
              |> Option.map ~f:Float.of_string
              |> Option.map ~f:Int.of_float
              |> Option.iter ~f:(fun n -> t.remaining_api_calls <- Known n);
              Cohttp.Header.get headers "X-Ratelimit-Reset"
              |> Option.map ~f:Int.of_string
              |> Option.map ~f:Time.Span.of_int_sec
              |> Option.map ~f:(Time.Span.( + ) tolerance)
              |> Option.iter ~f:(fun n -> t.reset_wait <- n);
              Ivar.fill ivar result));
      clear_queue t;
      Ivar.read ivar
    ;;
  end

  type t =
    { config : Config.t
    ; mutable access_token : Access_token.t option
    }
  [@@deriving sexp]

  let create config () = { config; access_token = None }

  let get_token t =
    let open Async.Let_syntax in
    let%bind _response, body =
      let uri = Uri.of_string "https://www.reddit.com/api/v1/access_token" in
      let headers =
        [ "Authorization", Config.basic_auth_string t.config ]
        |> add_user_agent
        |> Cohttp.Header.of_list
      in
      Cohttp_async.Client.post_form
        ~headers
        uri
        ~params:
          [ "grant_type", [ "password" ]
          ; "username", [ t.config.username ]
          ; "password", [ t.config.password ]
          ]
    in
    let%bind response_string = Cohttp_async.Body.to_string body in
    let response_json = Yojson.Basic.from_string response_string in
    let access_token : Access_token.t =
      let open Yojson.Basic.Util in
      let token = response_json |> member "access_token" |> to_string in
      let expiration =
        let additional_seconds =
          response_json |> member "expires_in" |> to_int |> Time.Span.of_int_sec
        in
        Time.add (Time.now ()) additional_seconds
      in
      { token; expiration }
    in
    t.access_token <- Some access_token;
    return ()
  ;;

  let call ?body t method_ uri =
    match t.access_token with
    | None -> raise_s [%message "no access token" (t : t)]
    | Some ({ token; _ } as access_token) ->
      let%bind () =
        match Access_token.is_almost_expired access_token with
        | true -> get_token t
        | false -> return ()
      in
      let headers =
        [ "Authorization", sprintf "bearer %s" token ]
        |> add_user_agent
        |> Cohttp.Header.of_list
      in
      Cohttp_async.Client.call ?body ~headers method_ uri
  ;;
end
