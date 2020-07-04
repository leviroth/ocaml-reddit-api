open! Core
open! Async

module Credentials = struct
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

let live_cohttp_client library_client_user_agent : (module Cohttp_client_wrapper) =
  (module struct
    let user_agent = library_client_user_agent ^ " OCaml Api Wrapper/0.1"
    let add_user_agent headers = Cohttp.Header.add headers "User-Agent" user_agent
    let get uri ~headers = Cohttp_async.Client.get uri ~headers:(add_user_agent headers)

    let post_form uri ~headers ~params =
      Cohttp_async.Client.post_form uri ~headers:(add_user_agent headers) ~params
    ;;
  end)
;;

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
    { credentials : Credentials.t
    ; mutable access_token : Access_token.t option
    }
  [@@deriving sexp]

  let create credentials () = { credentials; access_token = None }

  let get_token (module Cohttp_client_wrapper : Cohttp_client_wrapper) t ~time_source =
    let open Async.Let_syntax in
    let%bind _response, body =
      let uri = Uri.of_string "https://www.reddit.com/api/v1/access_token" in
      let headers =
        Cohttp.Header.init_with
          "Authorization"
          (Credentials.basic_auth_string t.credentials)
      in
      Cohttp_client_wrapper.post_form
        ~headers
        uri
        ~params:
          [ "grant_type", [ "password" ]
          ; "username", [ t.credentials.username ]
          ; "password", [ t.credentials.password ]
          ]
    in
    let%bind response_string = Cohttp_async.Body.to_string body in
    let response_json = Json.of_string response_string in
    let access_token : Access_token.t =
      let token = Json.find response_json ~key:"access_token" |> Json.get_string in
      let expiration =
        let additional_seconds =
          Json.find response_json ~key:"expires_in"
          |> Json.get_int
          |> Time_ns.Span.of_int_sec
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
    let headers = Cohttp.Header.add headers "Authorization" (sprintf "bearer %s" token) in
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
  ; cohttp_client_wrapper : ((module Cohttp_client_wrapper)[@sexp.opaque])
  ; time_source : Time_source.t
  ; more_children_sequencer : unit Sequencer.t
  }
[@@deriving sexp_of]

let create_internal cohttp_client_wrapper credentials ~time_source =
  { auth = Auth.create credentials ()
  ; rate_limiter = Rate_limiter.create ()
  ; cohttp_client_wrapper
  ; time_source
  ; more_children_sequencer = Sequencer.create ()
  }
;;

let create credentials ~user_agent =
  create_internal
    (live_cohttp_client user_agent)
    credentials
    ~time_source:(Time_source.wall_clock ())
;;

let more_children_sequencer t = t.more_children_sequencer

let with_t { auth; rate_limiter; cohttp_client_wrapper; time_source; _ } ~f ~headers =
  Auth.with_t auth ~headers ~cohttp_client_wrapper ~time_source ~f:(fun headers ->
      Rate_limiter.with_t rate_limiter ~time_source ~f:(fun () -> f headers))
;;

let post_form t uri ~params =
  let (module Cohttp_client_wrapper) = t.cohttp_client_wrapper in
  let headers = Cohttp.Header.init () in
  Monitor.try_with (fun () ->
      with_t t ~headers ~f:(fun headers ->
          Cohttp_client_wrapper.post_form ~headers ~params uri))
;;

let get t uri =
  let (module Cohttp_client_wrapper) = t.cohttp_client_wrapper in
  let headers = Cohttp.Header.init () in
  Monitor.try_with (fun () ->
      with_t t ~headers ~f:(fun headers -> Cohttp_client_wrapper.get ~headers uri))
;;

module For_testing = struct
  module Placeholders : sig
    type t

    val create : unit -> t
    val add : t -> secret:string -> placeholder:string -> unit
    val filter_string : t -> string -> string
    val insert_dummy_strings : t -> string -> string
  end = struct
    type t = string String.Table.t

    let create () = String.Table.create ()

    let add t ~secret ~placeholder =
      let placeholder = sprintf "<%s>" (String.uppercase placeholder) in
      Hashtbl.add_exn t ~key:placeholder ~data:secret
    ;;

    let filter_string t string =
      Hashtbl.fold t ~init:string ~f:(fun ~key:placeholder ~data:secret string ->
          String.substr_replace_all string ~pattern:secret ~with_:placeholder)
    ;;

    let insert_dummy_strings t string =
      Hashtbl.fold t ~init:string ~f:(fun ~key:placeholder ~data:secret string ->
          String.substr_replace_all string ~pattern:placeholder ~with_:secret)
    ;;
  end

  module type Cohttp_client_wrapper = Cohttp_client_wrapper

  let create = create_internal

  module Cassette = struct
    module type Wrapper = sig
      include Cohttp_client_wrapper

      val seal : unit -> unit
    end

    module Interaction = struct
      let map_headers headers ~f =
        Cohttp.Header.map (fun _key values -> List.map values ~f) headers
      ;;

      module Request = struct
        module T = struct
          type t =
            | Get of
                { uri : Uri_sexp.t
                ; headers : Cohttp.Header.t
                }
            | Post_form of
                { uri : Uri_sexp.t
                ; headers : Cohttp.Header.t
                ; params : (string * string list) list
                }
          [@@deriving sexp, compare]

          let uri (Get { uri; _ } | Post_form { uri; _ }) = uri
        end

        include T
        include Comparable.Make (T)

        let map t ~f =
          let map_uri uri = Uri.to_string uri |> f |> Uri.of_string in
          let map_params params =
            List.map params ~f:(fun (key, values) -> key, List.map values ~f)
          in
          match t with
          | Get { uri; headers } ->
            Get { uri = map_uri uri; headers = map_headers headers ~f }
          | Post_form { uri; headers; params } ->
            Post_form
              { uri = map_uri uri
              ; headers = map_headers headers ~f
              ; params = map_params params
              }
        ;;
      end

      type t =
        { request : Request.t
        ; response : Cohttp.Response.t * string
        }
      [@@deriving sexp]

      let map { request; response = response, body } ~f =
        let request = Request.map request ~f in
        let response =
          { response with headers = map_headers response.headers ~f }, f body
        in
        { request; response }
      ;;
    end

    let recording_wrapper filename placeholders : (module Wrapper) =
      (module struct
        module Cohttp_client_wrapper = (val live_cohttp_client "ocaml-reddit testing")

        let queue : Interaction.t Queue.t = Queue.create ()

        let save_interaction request response =
          let%bind response_to_write =
            let response, body = response in
            let%bind body = Cohttp_async.Body.to_string body in
            return (response, body)
          in
          Queue.enqueue queue { request; response = response_to_write };
          return (Tuple2.map_snd response_to_write ~f:Cohttp_async.Body.of_string)
        ;;

        let get uri ~headers =
          let%bind response = Cohttp_client_wrapper.get uri ~headers in
          let%bind response = save_interaction (Get { uri; headers }) response in
          return response
        ;;

        let post_form uri ~headers ~params =
          let%bind response = Cohttp_client_wrapper.post_form uri ~headers ~params in
          let%bind response =
            save_interaction (Post_form { uri; headers; params }) response
          in
          return response
        ;;

        let is_access_token_interaction (interaction : Interaction.t) =
          String.is_substring
            (Interaction.Request.uri interaction.request |> Uri.to_string)
            ~substring:"api/v1/access_token"
        ;;

        let seal () =
          Out_channel.with_file filename ~f:(fun out_channel ->
              Queue.iter queue ~f:(fun interaction ->
                  (match is_access_token_interaction interaction with
                  | false -> ()
                  | true ->
                    let _, body = interaction.response in
                    let json = Json.of_string body in
                    let token = Json.find json ~key:"access_token" |> Json.get_string in
                    Placeholders.add
                      placeholders
                      ~secret:token
                      ~placeholder:"access_token");
                  Interaction.map interaction ~f:(Placeholders.filter_string placeholders)
                  |> Interaction.sexp_of_t
                  |> Sexp.output_mach out_channel))
        ;;
      end)
    ;;

    let reading_wrapper filename placeholders : (module Wrapper) =
      (module struct
        let queue : Interaction.t Queue.t =
          In_channel.with_file filename ~f:(fun in_channel ->
              Sexp.input_sexps in_channel
              |> List.map ~f:Interaction.t_of_sexp
              |> Queue.of_list)
        ;;

        let dequeue_response () =
          let ({ request; response } : Interaction.t) =
            Queue.dequeue_exn queue
            |> Interaction.map ~f:(Placeholders.insert_dummy_strings placeholders)
          in
          let response = Tuple2.map_snd response ~f:Cohttp_async.Body.of_string in
          request, response
        ;;

        let get uri ~headers =
          let request, response = dequeue_response () in
          let fail () =
            raise_s
              [%message
                "Test request did not match record"
                  (uri : Uri_sexp.t)
                  (headers : Cohttp.Header.t)
                  ~recorded_request:(request : Interaction.Request.t)]
          in
          match request with
          | Post_form _ -> fail ()
          | Get request ->
            (match
               Uri.equal uri request.uri
               && [%compare.equal: Cohttp.Header.t] headers request.headers
             with
            | false -> fail ()
            | true -> return response)
        ;;

        let post_form uri ~headers ~params =
          let request, response = dequeue_response () in
          let fail () =
            raise_s
              [%message
                "Test request did not match record"
                  (uri : Uri_sexp.t)
                  (headers : Cohttp.Header.t)
                  (params : (string * string list) list)
                  ~recorded_request:(request : Interaction.Request.t)]
          in
          match request with
          | Get _ -> fail ()
          | Post_form request ->
            (match
               Uri.equal uri request.uri
               && [%compare.equal: Cohttp.Header.t] headers request.headers
               && [%equal: (string * string list) list] params request.params
             with
            | false -> fail ()
            | true -> return response)
        ;;

        let seal () = assert (Queue.is_empty queue)
      end)
    ;;

    let with_t filename ~credentials ~f =
      let placeholders = Placeholders.create () in
      let ({ client_id; client_secret; password; username } : Credentials.t) =
        credentials
      in
      Placeholders.add placeholders ~secret:client_id ~placeholder:"client_id";
      Placeholders.add placeholders ~secret:client_secret ~placeholder:"client_secret";
      Placeholders.add placeholders ~secret:password ~placeholder:"password";
      Placeholders.add placeholders ~secret:username ~placeholder:"username";
      Placeholders.add
        placeholders
        ~secret:(Credentials.basic_auth_string credentials)
        ~placeholder:"authorization";
      let%bind (module Wrapper) =
        match%bind Sys.file_exists_exn filename with
        | true -> return (reading_wrapper filename placeholders)
        | false -> return (recording_wrapper filename placeholders)
      in
      Monitor.protect
        (fun () -> f (module Wrapper : Cohttp_client_wrapper))
        ~finally:(fun () ->
          Wrapper.seal ();
          return ())
    ;;
  end
end
