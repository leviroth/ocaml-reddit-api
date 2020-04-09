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

module type Cohttp_client_wrapper = sig
  val get
    :  Uri.t
    -> headers:Cohttp.Header.t
    -> (Cohttp.Response.t * Cohttp_async.Body.t) Deferred.t

  val post_form
    :  Uri.t
    -> headers:Cohttp.Header.t
    -> params:(string * string list) list
    -> (Cohttp.Response.t * Cohttp_async.Body.t) Deferred.t
end

module Live_cohttp_client : Cohttp_client_wrapper = struct
  let get uri ~headers = Cohttp_async.Client.get uri ~headers
  let post_form uri ~headers ~params = Cohttp_async.Client.post_form uri ~headers ~params
end

module Auth = struct
  module Access_token = struct
    type t =
      { token : string
      ; expiration : Time_ns.t
      }
    [@@deriving sexp]

    let is_almost_expired { expiration; _ } ~time_source =
      let time_with_padding =
        Time_ns.add (Time_source.now time_source) (Time_ns.Span.of_int_sec 10)
      in
      Time_ns.( <= ) expiration time_with_padding
    ;;
  end

  type t =
    { config : Config.t
    ; mutable access_token : Access_token.t option
    }
  [@@deriving sexp]

  let create config () = { config; access_token = None }

  let get_token (module Cohttp_client_wrapper : Cohttp_client_wrapper) t ~time_source =
    let open Async.Let_syntax in
    let%bind _response, body =
      let uri = Uri.of_string "https://www.reddit.com/api/v1/access_token" in
      let headers =
        [ "Authorization", Config.basic_auth_string t.config ]
        |> add_user_agent
        |> Cohttp.Header.of_list
      in
      Cohttp_client_wrapper.post_form
        ~headers
        uri
        ~params:
          [ "grant_type", [ "password" ]
          ; "username", [ t.config.username ]
          ; "password", [ t.config.password ]
          ]
    in
    let%bind response_string = Cohttp_async.Body.to_string body in
    let response_json = Yojson.Safe.from_string response_string in
    let access_token : Access_token.t =
      let open Yojson.Safe.Util in
      let token = response_json |> member "access_token" |> to_string in
      let expiration =
        let additional_seconds =
          response_json |> member "expires_in" |> to_int |> Time_ns.Span.of_int_sec
        in
        Time_ns.add (Time_source.now time_source) additional_seconds
      in
      { token; expiration }
    in
    t.access_token <- Some access_token;
    return access_token
  ;;

  let with_t t ~f ~headers ~cohttp_client_wrapper ~time_source =
    let%bind access_token =
      match t.access_token with
      | None -> get_token cohttp_client_wrapper t ~time_source
      | Some access_token -> return access_token
    in
    let%bind { token; _ } =
      match Access_token.is_almost_expired access_token ~time_source with
      | true -> get_token cohttp_client_wrapper t ~time_source
      | false -> return access_token
    in
    let headers =
      [ "Authorization", sprintf "bearer %s" token ]
      |> add_user_agent
      |> Cohttp.Header.add_list headers
    in
    f headers
  ;;
end

module Rate_limiter : sig
  type t [@@deriving sexp]

  val create : unit -> t

  val with_t
    :  t
    -> f:(unit -> (Cohttp.Response.t * Cohttp_async.Body.t) Deferred.t)
    -> time_source:Time_source.t
    -> (Cohttp.Response.t * Cohttp_async.Body.t) Deferred.t
end = struct
  module Server_side_info = struct
    type t =
      { remaining_api_calls : int
      ; reset_time : Time_ns.t
      }
    [@@deriving sexp, fields]

    let t_of_headers headers ~time_source =
      let open Option.Let_syntax in
      let%bind remaining_api_calls =
        Cohttp.Header.get headers "X-Ratelimit-Remaining"
        >>| Float.of_string
        >>| Int.of_float
      and reset_time =
        let%bind relative_reset_time =
          Cohttp.Header.get headers "X-Ratelimit-Reset"
          >>| Int.of_string
          >>| Time_ns.Span.of_int_sec
        in
        return (Time_ns.add (Time_source.now time_source) relative_reset_time)
      in
      return { remaining_api_calls; reset_time }
    ;;

    let compare_approximate_reset_times time1 time2 =
      let tolerance = Time_ns.Span.of_int_sec 60 in
      let lower = Maybe_bound.Incl (Time_ns.sub time2 tolerance) in
      let upper = Maybe_bound.Incl (Time_ns.add time2 tolerance) in
      match
        Maybe_bound.compare_to_interval_exn time1 ~lower ~upper ~compare:Time_ns.compare
      with
      | Below_lower_bound -> -1
      | In_range -> 0
      | Above_upper_bound -> 1
    ;;

    let demonstrates_reset old_t new_t =
      match
        compare_approximate_reset_times old_t.reset_time new_t.reset_time
        |> Ordering.of_int
      with
      | Less -> true
      | Greater | Equal -> false
    ;;

    let compare_by_inferred_time_on_server t t' =
      Comparable.lexicographic
        [ Comparable.lift compare_approximate_reset_times ~f:reset_time
        ; Fn.flip (Comparable.lift compare ~f:remaining_api_calls)
        ]
        t
        t'
    ;;

    let update t t' =
      match compare_by_inferred_time_on_server t t' |> Ordering.of_int with
      | Less | Equal -> t'
      | Greater -> t
    ;;
  end

  type t =
    { mutable server_side_info : Server_side_info.t option
    ; mutable waiting_for_reset : bool
    ; jobs : (unit -> unit) Queue.t
    }
  [@@deriving sexp]

  let create () =
    { server_side_info = None; waiting_for_reset = false; jobs = Queue.create () }
  ;;

  let rec clear_queue t ~time_source =
    match t.waiting_for_reset with
    | true -> ()
    | false ->
      Queue.dequeue t.jobs
      |> Option.iter ~f:(fun job ->
             match t.server_side_info with
             | None ->
               t.waiting_for_reset <- true;
               job ()
             | Some ({ reset_time; remaining_api_calls } as server_side_info) ->
               (match remaining_api_calls with
               | 0 ->
                 t.waiting_for_reset <- true;
                 upon (Time_source.at time_source reset_time) job
               | n ->
                 t.server_side_info
                   <- Some { server_side_info with remaining_api_calls = n - 1 };
                 job ();
                 clear_queue t ~time_source))
  ;;

  let update_server_side_info t new_server_side_info =
    t.server_side_info
      <- Some
           (match t.server_side_info with
           | None -> new_server_side_info
           | Some server_side_info ->
             Server_side_info.update server_side_info new_server_side_info)
  ;;

  let with_t t ~f ~time_source =
    let ivar = Ivar.create () in
    Queue.enqueue t.jobs (fun () ->
        upon (f ()) (fun ((response, _body) as result) ->
            let headers = Cohttp.Response.headers response in
            Server_side_info.t_of_headers headers ~time_source
            |> Option.iter ~f:(fun new_server_side_info ->
                   (match t.server_side_info with
                   | None -> t.waiting_for_reset <- false
                   | Some old_server_side_info ->
                     (match
                        Server_side_info.demonstrates_reset
                          old_server_side_info
                          new_server_side_info
                      with
                     | false -> ()
                     | true -> t.waiting_for_reset <- false));
                   update_server_side_info t new_server_side_info);
            Ivar.fill ivar result;
            clear_queue t ~time_source));
    clear_queue t ~time_source;
    Ivar.read ivar
  ;;
end

type t =
  { auth : Auth.t
  ; rate_limiter : Rate_limiter.t
  ; cohttp_client_wrapper : (module Cohttp_client_wrapper) sexp_opaque
  ; time_source : Time_source.t
  }
[@@deriving sexp_of]

let create_internal cohttp_client_wrapper config ~time_source =
  { auth = Auth.create config ()
  ; rate_limiter = Rate_limiter.create ()
  ; cohttp_client_wrapper
  ; time_source
  }
;;

let create config =
  create_internal
    (module Live_cohttp_client)
    config
    ~time_source:(Time_source.wall_clock ())
;;

let with_t t ~f ~headers ~cohttp_client_wrapper ~time_source =
  Auth.with_t t.auth ~headers ~cohttp_client_wrapper ~time_source ~f:(fun headers ->
      Rate_limiter.with_t t.rate_limiter ~time_source:t.time_source ~f:(fun () ->
          f headers))
;;

let with_retry_internal ?(allowed_exception_retries = 3) f =
  let next_time_to_wait time =
    Time_ns.Span.(max (Time_ns.Span.scale_int time 2) Time_ns.Span.minute)
  in
  let rec with_retry f time_to_wait allowed_exception_retries =
    match%bind try_with f with
    | Ok ((response, _body) as result) ->
      (match Cohttp.Response.status response with
      | #Cohttp.Code.server_error_status ->
        Log.Global.error_s
          [%message "got server error status code" (response : Cohttp.Response.t)];
        with_retry f (next_time_to_wait time_to_wait) allowed_exception_retries
      | _ -> return result)
    | Error exn ->
      (match allowed_exception_retries with
      | 0 ->
        raise_s
          [%message "Saw repeated exceptions while retrying HTTP request" (exn : Exn.t)]
      | n ->
        Log.Global.error_s [%message "saw exception" (exn : Exn.t)];
        with_retry f (next_time_to_wait time_to_wait) (n - 1))
  in
  with_retry f Time_ns.Span.second allowed_exception_retries
;;

let with_retry t ~f ~headers =
  with_retry_internal (fun () ->
      with_t
        t
        ~f
        ~headers
        ~cohttp_client_wrapper:t.cohttp_client_wrapper
        ~time_source:t.time_source)
;;

let post_form t uri ~params =
  let (module Cohttp_client_wrapper) = t.cohttp_client_wrapper in
  let headers = Cohttp.Header.init () in
  with_retry
    t
    ~f:(fun headers -> Cohttp_client_wrapper.post_form ~headers ~params uri)
    ~headers
;;

let get t uri =
  let (module Cohttp_client_wrapper) = t.cohttp_client_wrapper in
  let headers = Cohttp.Header.init () in
  with_retry t ~f:(fun headers -> Cohttp_client_wrapper.get ~headers uri) ~headers
;;

module For_testing = struct
  module type Cohttp_client_wrapper = Cohttp_client_wrapper

  let create = create_internal
end
