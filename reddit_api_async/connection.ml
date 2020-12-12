open! Core
open! Async
open Reddit_api_kernel

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

module Sequencer_table = Sequencer_table.Make (Api.Sequencer)

module type T = sig
  type t [@@deriving sexp_of]

  val post_form
    :  ?sequence:Api.Sequencer.t
    -> t
    -> Uri.t
    -> params:(string * string list) list
    -> (Cohttp.Response.t * Cohttp_async.Body.t, Exn.t) Deferred.Result.t

  val get
    :  ?sequence:Api.Sequencer.t
    -> t
    -> Uri.t
    -> (Cohttp.Response.t * Cohttp_async.Body.t, Exn.t) Deferred.Result.t
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

module Local = struct
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
        let token = Json.find response_json [ "access_token" ] |> Json.get_string in
        let expiration =
          let additional_seconds =
            Json.find response_json [ "expires_in" ]
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
      let headers =
        Cohttp.Header.add headers "Authorization" (sprintf "bearer %s" token)
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

      let snap_to_nearest_minute time =
        let interval = Time_ns.Span.minute in
        let base = Time_ns.epoch in
        let candidates =
          [ Time_ns.prev_multiple ~can_equal_before:true ~interval ~base ~before:time ()
          ; Time_ns.next_multiple ~can_equal_after:false ~interval ~base ~after:time ()
          ]
        in
        List.min_elt
          candidates
          ~compare:
            (Comparable.lift Time_ns.Span.compare ~f:(fun time' ->
                 Time_ns.abs_diff time time'))
        |> Option.value_exn
      ;;

      let%expect_test _ =
        List.iter [ "2020-11-30 18:48:01.02Z"; "2020-11-30 18:47:59.02Z" ] ~f:(fun time ->
            let time = Time_ns.of_string time in
            print_s [%sexp (snap_to_nearest_minute time : Time_ns.Alternate_sexp.t)]);
        [%expect {|
          "2020-11-30 18:48:00Z"
          "2020-11-30 18:48:00Z" |}]
      ;;

      let parse_http_header_date date_string =
        Scanf.sscanf
          date_string
          "%3s, %2d %3s %4d %2d:%2d:%2d GMT"
          (fun day_of_week d month y hr min sec ->
            let day_of_week = Day_of_week.of_string day_of_week in
            let month = Month.of_string month in
            let date = Date.create_exn ~y ~m:month ~d in
            (match Day_of_week.equal day_of_week (Date.day_of_week date) with
            | true -> ()
            | false ->
              raise_s
                [%message
                  "HTTP response: Day of week did not match parsed date"
                    (day_of_week : Day_of_week.t)
                    (date : Date.t)
                    (date_string : string)]);
            let ofday = Time_ns.Ofday.create ~hr ~min ~sec () in
            Time_ns.of_date_ofday date ofday ~zone:Time_ns.Zone.utc)
      ;;

      let%expect_test _ =
        print_s
          [%sexp
            (parse_http_header_date "Wed, 21 Oct 2015 07:28:00 GMT"
              : Time_ns.Alternate_sexp.t)];
        [%expect {| "2015-10-21 07:28:00Z" |}]
      ;;

      let t_of_headers headers =
        let get_header header =
          match Cohttp.Header.get headers header with
          | Some v -> v
          | None ->
            raise_s
              [%message
                "Missing expected X-Ratelimit header"
                  (header : string)
                  (headers : Cohttp.Header.t)]
        in
        let remaining_api_calls =
          get_header "X-Ratelimit-Remaining" |> Float.of_string |> Int.of_float
        in
        let reset_time =
          let absolute_time = parse_http_header_date (get_header "Date") in
          let relative_reset_time =
            get_header "X-Ratelimit-Reset" |> Int.of_string |> Time_ns.Span.of_int_sec
          in
          snap_to_nearest_minute (Time_ns.add absolute_time relative_reset_time)
        in
        { remaining_api_calls; reset_time }
      ;;

      let demonstrates_reset old_t new_t = Time_ns.( < ) old_t.reset_time new_t.reset_time

      let compare_by_inferred_time_on_server =
        Comparable.lexicographic
          [ Comparable.lift Time_ns.compare ~f:reset_time
          ; Comparable.reverse (Comparable.lift compare ~f:remaining_api_calls)
          ]
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
      Deferred.create (fun ivar ->
          Queue.enqueue t.jobs (fun () ->
              upon (f ()) (fun ((response, _body) as result) ->
                  let headers = Cohttp.Response.headers response in
                  let new_server_side_info = Server_side_info.t_of_headers headers in
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
                  update_server_side_info t new_server_side_info;
                  Ivar.fill ivar result;
                  clear_queue t ~time_source));
          clear_queue t ~time_source)
    ;;
  end

  type t =
    { auth : Auth.t
    ; rate_limiter : Rate_limiter.t
    ; cohttp_client_wrapper : ((module Cohttp_client_wrapper)[@sexp.opaque])
    ; time_source : Time_source.t
    ; sequencer_table : (Nothing.t, Nothing.t) Sequencer_table.t
    }
  [@@deriving sexp_of]

  let create_internal cohttp_client_wrapper credentials ~time_source =
    { auth = Auth.create credentials ()
    ; rate_limiter = Rate_limiter.create ()
    ; cohttp_client_wrapper
    ; time_source
    ; sequencer_table = Sequencer_table.create ()
    }
  ;;

  let create credentials ~user_agent =
    create_internal
      (live_cohttp_client user_agent)
      credentials
      ~time_source:(Time_source.wall_clock ())
  ;;

  let with_t
      ?sequence
      { auth; rate_limiter; cohttp_client_wrapper; time_source; sequencer_table }
      ~f
      ~headers
    =
    let run (None : Nothing.t option) =
      Auth.with_t auth ~headers ~cohttp_client_wrapper ~time_source ~f:(fun headers ->
          Rate_limiter.with_t rate_limiter ~time_source ~f:(fun () -> f headers))
    in
    match sequence with
    | None -> run None
    | Some sequencer -> Sequencer_table.enqueue sequencer_table ~key:sequencer run
  ;;

  let post_form ?sequence t uri ~params =
    let (module Cohttp_client_wrapper) = t.cohttp_client_wrapper in
    let headers = Cohttp.Header.init () in
    Monitor.try_with (fun () ->
        with_t ?sequence t ~headers ~f:(fun headers ->
            Cohttp_client_wrapper.post_form ~headers ~params uri))
  ;;

  let get ?sequence t uri =
    let (module Cohttp_client_wrapper) = t.cohttp_client_wrapper in
    let headers = Cohttp.Header.init () in
    Monitor.try_with (fun () ->
        with_t ?sequence t ~headers ~f:(fun headers ->
            Cohttp_client_wrapper.get ~headers uri))
  ;;
end

type t = T : (module T with type t = 't) * 't -> t

let sexp_of_t (T ((module T), t)) = T.sexp_of_t t

let create credentials ~user_agent =
  T ((module Local), Local.create credentials ~user_agent)
;;

let get ?sequence (T ((module T), t)) = T.get ?sequence t
let post_form ?sequence (T ((module T), t)) = T.post_form ?sequence t

let call_raw t ({ request; sequencer = sequence; handle_response = _ } : _ Api.t) =
  match%bind
    match request with
    | Get { uri } -> get ?sequence t uri
    | Post_form { uri; params } -> post_form ?sequence t uri ~params
  with
  | Ok (response, body) ->
    let%bind body = Cohttp_async.Body.to_string body >>| Cohttp.Body.of_string in
    return (Ok (response, body))
  | Error exn -> return (Error exn)
;;

let call t api =
  match%bind call_raw t api with
  | Ok (response, body) -> return (api.handle_response (response, body))
  | Error exn -> return (Error (Api.Api_error.Cohttp_raised exn))
;;

let call_exn t api =
  match%bind call t api with
  | Ok v -> return v
  | Error (Cohttp_raised exn) -> raise exn
  | Error (Reddit_reported_error (response, body)) ->
    raise_s [%message "HTTP error" (response : Cohttp.Response.t) (body : Cohttp.Body.t)]
;;

module Remote = struct
  module Protocol = struct
    module Cohttp_response = struct
      module T = struct
        include Cohttp.Response
        module Binable = Sexp

        let to_binable = Cohttp.Response.sexp_of_t
        let of_binable = Cohttp.Response.t_of_sexp

        let caller_identity =
          Bin_prot.Shape.Uuid.of_string "013b732e-e3fc-11ea-95d1-ffe802709160"
        ;;
      end

      include T
      include Bin_prot.Utils.Make_binable_with_uuid (T)
    end

    module Uri = struct
      module T = struct
        include Uri
        module Binable = String

        let to_binable v = Uri.to_string v
        let of_binable = Uri.of_string

        let caller_identity =
          Bin_prot.Shape.Uuid.of_string "118c15e0-e400-11ea-b4dc-6bc3e7e7983c"
        ;;
      end

      include T
      include Bin_prot.Utils.Make_binable_with_uuid (T)
    end

    module Exn = struct
      module T = struct
        include Exn
        module Binable = Error

        let to_binable t = Error.of_exn ~backtrace:`Get t
        let of_binable = Error.to_exn

        let caller_identity =
          Bin_prot.Shape.Uuid.of_string "dffaba84-e410-11ea-ad49-0755ab1141a3"
        ;;
      end

      include T
      include Bin_prot.Utils.Make_binable_with_uuid (T)
    end

    let get =
      Rpc.Rpc.create
        ~name:"get"
        ~version:1
        ~bin_query:[%bin_type_class: Api.Sequencer.t option * Uri.t]
        ~bin_response:[%bin_type_class: (Cohttp_response.t * string, Exn.t) Result.t]
    ;;

    let post_form =
      Rpc.Rpc.create
        ~name:"post_form"
        ~version:1
        ~bin_query:
          [%bin_type_class: Api.Sequencer.t option * Uri.t * (string * string list) list]
        ~bin_response:[%bin_type_class: (Cohttp_response.t * string, Exn.t) Result.t]
    ;;
  end

  module Client = struct
    type t = Rpc.Connection.t [@@deriving sexp_of]

    let get_body v =
      let open Deferred.Result.Let_syntax in
      let%bind response, body = v in
      let body = `String body in
      return (response, body)
    ;;

    let get ?sequence t uri =
      get_body (Rpc.Rpc.dispatch_exn Protocol.get t (sequence, uri))
    ;;

    let post_form ?sequence t uri ~params =
      get_body (Rpc.Rpc.dispatch_exn Protocol.post_form t (sequence, uri, params))
    ;;
  end

  module Server = struct
    let get_body v =
      let open Deferred.Result.Let_syntax in
      let%bind response, body = v in
      let%bind body = Cohttp_async.Body.to_string body |> Deferred.ok in
      return (response, body)
    ;;

    let get =
      Rpc.Rpc.implement Protocol.get (fun t (sequence, uri) ->
          get_body (get ?sequence t uri))
    ;;

    let post_form =
      Rpc.Rpc.implement Protocol.post_form (fun t (sequence, uri, params) ->
          get_body (post_form ?sequence t uri ~params))
    ;;

    let implementations =
      Rpc.Implementations.create_exn
        ~implementations:[ get; post_form ]
        ~on_unknown_rpc:`Close_connection
    ;;

    let serve t ~where_to_listen =
      Rpc.Connection.serve
        ~implementations
        ~initial_connection_state:(fun _ _ -> t)
        ~where_to_listen
        ()
    ;;
  end

  let serve = Server.serve

  let connect_exn where_to_connect =
    let%bind t = Rpc.Connection.client where_to_connect >>| Result.ok_exn in
    return (T ((module Client), t))
  ;;
end

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

  module Cassette = struct
    module type S = sig
      include Cohttp_client_wrapper

      val seal : unit -> unit
      val read_only_time_source : Time_source.t
      val read_write_time_source : Time_source.Read_write.t option
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

    let recording filename placeholders : (module S) =
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
          printf "Please move the following to test/%s\n\n" filename;
          Queue.iter queue ~f:(fun interaction ->
              (match is_access_token_interaction interaction with
              | false -> ()
              | true ->
                let _, body = interaction.response in
                let json = Json.of_string body in
                let token = Json.find json [ "access_token" ] |> Json.get_string in
                Placeholders.add placeholders ~secret:token ~placeholder:"access_token");
              Interaction.map interaction ~f:(Placeholders.filter_string placeholders)
              |> Interaction.sexp_of_t
              |> Sexp.output_mach Out_channel.stdout);
          printf "\n\nPlease move the above to test/%s" filename
        ;;

        let read_only_time_source = Time_source.wall_clock ()
        let read_write_time_source = None
      end)
    ;;

    let reading filename placeholders : (module S) =
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
        let time_source = Time_source.create ~now:Time_ns.epoch ()
        let read_write_time_source = Some time_source
        let read_only_time_source = Time_source.read_only time_source
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
      let%bind (module Cassette) =
        let%bind make =
          match%bind Sys.file_exists_exn filename with
          | true -> return reading
          | false -> return recording
        in
        return (make filename placeholders)
      in
      Monitor.protect
        (fun () -> f (module Cassette : S))
        ~finally:(fun () ->
          Cassette.seal ();
          return ())
    ;;
  end

  let with_cassette filename ~credentials ~f =
    Cassette.with_t filename ~credentials ~f:(fun (module Cassette : Cassette.S) ->
        let connection =
          T
            ( (module Local)
            , Local.create_internal
                (module Cassette)
                credentials
                ~time_source:Cassette.read_only_time_source )
        in
        f connection)
  ;;
end
