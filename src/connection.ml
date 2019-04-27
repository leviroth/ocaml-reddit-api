open! Core
open! Async

let add_user_agent l =
  ("User-Agent", "OCaml Api Wrapper/0.1 - developed by /u/L72_Elite_kraken") :: l
;;

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

module Auth = struct
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
    return access_token
  ;;

  let call ?body t method_ uri =
    let%bind access_token =
      match t.access_token with
      | None -> get_token t
      | Some access_token -> return access_token
    in
    let%bind { token; _ } =
      match Access_token.is_almost_expired access_token with
      | true -> get_token t
      | false -> return access_token
    in
    let headers =
      [ "Authorization", sprintf "bearer %s" token ]
      |> add_user_agent
      |> Cohttp.Header.of_list
    in
    Cohttp_async.Client.call ?body ~headers method_ uri
  ;;
end

module Rate_limiter : sig
  type t [@@deriving sexp]

  val create : unit -> t

  val with_t
    :  t
    -> f:(unit -> (Cohttp.Response.t * Cohttp_async.Body.t) Deferred.t)
    -> (Cohttp.Response.t * Cohttp_async.Body.t) Deferred.t
end = struct
  let tolerance = Time.Span.of_int_sec 10

  type t =
    { mutable remaining_api_calls : int
    ; mutable reset_time : Time.t option
    ; mutable waiting_for_reset : bool
    ; jobs : (unit -> unit) Queue.t
    }
  [@@deriving sexp]

  let create () =
    { remaining_api_calls = 600
    ; reset_time = None
    ; waiting_for_reset = false
    ; jobs = Queue.create ()
    }
  ;;

  let rec clear_queue t =
    match t.waiting_for_reset with
    | true -> ()
    | false ->
      Queue.dequeue t.jobs
      |> Option.iter ~f:(fun job ->
             match t.remaining_api_calls with
             | 0 ->
               (match t.reset_time with
               | None ->
                 failwith
                   "Tried to run job with no remaining API calls and unknown reset time"
               (* TODO Bug *)
               | Some reset_time ->
                 t.waiting_for_reset <- true;
                 upon (at reset_time) job)
             | n ->
               t.remaining_api_calls <- n - 1;
               job ();
               clear_queue t)
  ;;

  let with_t t ~f =
    let ivar = Ivar.create () in
    Queue.enqueue t.jobs (fun () ->
        upon (f ()) (fun ((response, _body) as result) ->
            let headers = Cohttp.Response.headers response in
            Cohttp.Header.get headers "X-Ratelimit-Remaining"
            |> Option.iter ~f:(fun s ->
                   let remaining_api_calls = Float.of_string s |> Int.of_float in
                   t.remaining_api_calls <- remaining_api_calls);
            Cohttp.Header.get headers "X-Ratelimit-Reset"
            |> Option.iter ~f:(fun s ->
                   let reset_time =
                     Int.of_string s
                     |> Time.Span.of_int_sec
                     |> Time.Span.( + ) tolerance
                     |> Time.add (Time.now ())
                   in
                   t.reset_time <- Some reset_time);
            t.waiting_for_reset <- false;
            Ivar.fill ivar result;
            clear_queue t));
    clear_queue t;
    Ivar.read ivar
  ;;
end

type t =
  { auth : Auth.t
  ; rate_limiter : Rate_limiter.t
  }
[@@deriving sexp]

let create config =
  { auth = Auth.create config (); rate_limiter = Rate_limiter.create () }
;;

let call ?body t method_ uri =
  Rate_limiter.with_t t.rate_limiter ~f:(fun () -> Auth.call ?body t.auth method_ uri)
;;
