open! Core
open! Async
open Reddit_api_kernel

module Credentials = struct
  module Password = struct
    type t =
      { client_id : string
      ; client_secret : string
      ; password : string
      ; username : string
      }
    [@@deriving sexp]
  end

  module Refresh_token = struct
    type t =
      { client_id : string
      ; client_secret : string option
      ; refresh_token : string
      }
    [@@deriving sexp]
  end

  module Userless_confidential = struct
    type t =
      { client_id : string
      ; client_secret : string
      }
    [@@deriving sexp]
  end

  module Userless_public = struct
    type t =
      { client_id : string
      ; device_id : string option
      }
    [@@deriving sexp]

    let device_id_or_default t =
      Option.value t.device_id ~default:"DO_NOT_TRACK_THIS_DEVICE"
    ;;
  end

  type t =
    | Password of Password.t
    | Refresh_token of Refresh_token.t
    | Userless_confidential of Userless_confidential.t
    | Userless_public of Userless_public.t
  [@@deriving sexp]

  let client_id t =
    match t with
    | Password { client_id; _ }
    | Refresh_token { client_id; _ }
    | Userless_confidential { client_id; _ }
    | Userless_public { client_id; _ } -> client_id
  ;;

  let client_secret t =
    match t with
    | Refresh_token { client_secret = None; _ } | Userless_public _ -> None
    | Password { client_secret; _ }
    | Refresh_token { client_secret = Some client_secret; _ }
    | Userless_confidential { client_secret; _ } -> Some client_secret
  ;;

  let basic_auth_string t =
    let client_id = client_id t in
    let client_secret = Option.value (client_secret t) ~default:"" in
    Cohttp.Auth.string_of_credential (`Basic (client_id, client_secret))
  ;;

  let auth_header t = Cohttp.Header.init_with "Authorization" (basic_auth_string t)

  let access_token_request_params t =
    match t with
    | Password { username; password; _ } ->
      [ "grant_type", [ "password" ]; "username", [ username ]; "password", [ password ] ]
    | Refresh_token { refresh_token; _ } ->
      [ "grant_type", [ "refresh_token" ]; "refresh_token", [ refresh_token ] ]
    | Userless_confidential _ -> [ "grant_type", [ "client_credentials" ] ]
    | Userless_public public_credentials ->
      [ "grant_type", [ "https://oauth.reddit.com/grants/installed_client" ]
      ; "device_id", [ Userless_public.device_id_or_default public_credentials ]
      ]
  ;;
end

module Sequencer_table = Sequencer_table.Make (Endpoint.Sequencer)

module Access_token_request_error = struct
  type t =
    | Cohttp_raised of Exn.t
    | Json_parsing_error of
        { error : Error.t
        ; response : Cohttp.Response.t
        ; body : Cohttp.Body.t
        }
    | Token_request_rejected of
        { response : Cohttp.Response.t
        ; body : Cohttp.Body.t
        }
    | Other_http_error of
        { response : Cohttp.Response.t
        ; body : Cohttp.Body.t
        }
  [@@deriving sexp_of]
end

module Error = struct
  type 'endpoint_error t =
    | Access_token_request_error of Access_token_request_error.t
    | Endpoint_error of 'endpoint_error
  [@@deriving sexp_of]
end

module type T = sig
  type t [@@deriving sexp_of]

  val post_form
    :  ?sequence:Endpoint.Sequencer.t
    -> t
    -> Uri.t
    -> params:(string * string list) list
    -> (Cohttp.Response.t * Cohttp_async.Body.t, Exn.t Error.t) Deferred.Result.t

  val get
    :  ?sequence:Endpoint.Sequencer.t
    -> t
    -> Uri.t
    -> (Cohttp.Response.t * Cohttp_async.Body.t, Exn.t Error.t) Deferred.Result.t

  val set_access_token : t -> token:string -> expiration:Time_ns.t -> unit
end

module type Cohttp_client_wrapper = sig
  val get
    :  Uri.t
    -> headers:Cohttp.Header.t
    -> (Cohttp.Response.t * Cohttp_async.Body.t, Exn.t) Deferred.Result.t

  val post_form
    :  Uri.t
    -> headers:Cohttp.Header.t
    -> params:(string * string list) list
    -> (Cohttp.Response.t * Cohttp_async.Body.t, Exn.t) Deferred.Result.t
end

let live_cohttp_client library_client_user_agent : (module Cohttp_client_wrapper) =
  (module struct
    let user_agent = library_client_user_agent ^ " ocaml-reddit-api/0.1.1"
    let add_user_agent headers = Cohttp.Header.add headers "User-Agent" user_agent

    let get uri ~headers =
      Monitor.try_with (fun () ->
          Cohttp_async.Client.get uri ~headers:(add_user_agent headers))
    ;;

    let post_form uri ~headers ~params =
      Monitor.try_with (fun () ->
          Cohttp_async.Client.post_form uri ~headers:(add_user_agent headers) ~params)
    ;;
  end)
;;

module Local = struct
  module Auth = struct
    module Access_token = struct
      type t =
        { token : string
        ; expiration : Time_ns_unix.t
        }
      [@@deriving sexp]

      let is_almost_expired { expiration; _ } ~time_source =
        let time_with_padding =
          Time_ns.add (Time_source.now time_source) (Time_ns.Span.of_int_sec 10)
        in
        Time_ns.( <= ) expiration time_with_padding
      ;;
    end

    module Access_token_state = struct
      type t =
        | No_outstanding_request of Access_token.t option
        | Outstanding_request of
            (Access_token.t, Access_token_request_error.t) Result.t Ivar.t
      [@@deriving sexp_of]
    end

    type t =
      { credentials : Credentials.t
      ; mutable access_token : Access_token_state.t
      }
    [@@deriving sexp_of]

    let create credentials () =
      { credentials; access_token = No_outstanding_request None }
    ;;

    let get_token
        (module Cohttp_client_wrapper : Cohttp_client_wrapper)
        credentials
        ~time_source
      =
      match%bind
        let open Deferred.Result.Let_syntax in
        let%bind response, body =
          let uri = Uri.of_string "https://www.reddit.com/api/v1/access_token" in
          let headers = Credentials.auth_header credentials in
          let params = Credentials.access_token_request_params credentials in
          Cohttp_client_wrapper.post_form ~headers uri ~params
        in
        let%bind body_string =
          Monitor.try_with (fun () -> Cohttp_async.Body.to_string body)
        in
        return (response, body_string)
      with
      | Error exn -> return (Error (Access_token_request_error.Cohttp_raised exn))
      | Ok (response, body_string) ->
        let result : (Access_token.t, Access_token_request_error.t) Result.t =
          match Cohttp.Response.status response with
          | `Bad_request | `Unauthorized ->
            Error
              (Token_request_rejected
                 { response; body = Cohttp.Body.of_string body_string })
          | `OK ->
            (match Jsonaf.parse body_string with
            | Error error ->
              Error
                (Json_parsing_error
                   { error; response; body = Cohttp.Body.of_string body_string })
            | Ok response_json ->
              let token =
                Jsonaf.member_exn "access_token" response_json |> Jsonaf.string_exn
              in
              let expiration =
                let additional_seconds =
                  Jsonaf.member_exn "expires_in" response_json
                  |> Jsonaf.float_exn
                  |> Time_ns.Span.of_sec
                in
                Time_ns.add (Time_source.now time_source) additional_seconds
              in
              Ok { token; expiration })
          | _ ->
            Error
              (Other_http_error { response; body = Cohttp.Body.of_string body_string })
        in
        return result
    ;;

    let add_access_token t ~headers ~cohttp_client_wrapper ~time_source =
      let get_token () =
        let ivar = Ivar.create () in
        t.access_token <- Outstanding_request ivar;
        let%bind result = get_token cohttp_client_wrapper t.credentials ~time_source in
        t.access_token <- No_outstanding_request (Result.ok result);
        Ivar.fill ivar result;
        return result
      in
      let%bind result =
        match t.access_token with
        | Outstanding_request ivar -> Ivar.read ivar
        | No_outstanding_request None -> get_token ()
        | No_outstanding_request (Some access_token) ->
          (match Access_token.is_almost_expired access_token ~time_source with
          | false -> return (Ok access_token)
          | true -> get_token ())
      in
      match result with
      | Error _ as error -> return error
      | Ok { token; _ } ->
        return
          (Ok (Cohttp.Header.add headers "Authorization" (sprintf "bearer %s" token)))
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

  let create_internal cohttp_client_wrapper credentials ~time_source ~rate_limiter =
    { auth = Auth.create credentials ()
    ; rate_limiter
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

  let repeat_until_finished_with_result
      (state : 'state)
      (f : 'state -> ([ `Repeat of 'state | `Finished of 'ok ], 'error) Deferred.Result.t)
      : ('ok, 'error) Deferred.Result.t
    =
    Deferred.repeat_until_finished state (fun state ->
        match%bind f state with
        | Ok (`Repeat state) -> return (`Repeat state)
        | Ok (`Finished result) -> return (`Finished (Ok result))
        | Error result -> return (`Finished (Error result)))
  ;;

  let handle_request
      ?sequence
      { auth; rate_limiter; cohttp_client_wrapper; time_source; sequencer_table }
      ~f
      ~headers:initial_headers
    =
    let run () =
      repeat_until_finished_with_result () (fun () ->
          let open Deferred.Result.Let_syntax in
          let%bind headers =
            Auth.add_access_token
              auth
              ~headers:initial_headers
              ~cohttp_client_wrapper
              ~time_source
            |> Deferred.Result.map_error ~f:(fun error ->
                   Error.Access_token_request_error error)
          in
          let%bind () = Deferred.ok (Rate_limiter.permit_request rate_limiter) in
          let%bind ((response, _body) as result) = f headers in
          let authorization_failed =
            match Cohttp.Response.status response with
            | `Unauthorized ->
              (match
                 Cohttp.Header.get (Cohttp.Response.headers response) "www-authenticate"
               with
              | Some "Bearer realm=\"reddit\", error=\"invalid_token\"" -> true
              | Some _ | None -> false)
            | _ -> false
          in
          Rate_limiter.notify_response rate_limiter response;
          match authorization_failed with
          | false -> return (`Finished result)
          | true ->
            auth.access_token <- No_outstanding_request None;
            return (`Repeat ()))
    in
    match sequence with
    | None -> run ()
    | Some sequencer ->
      Sequencer_table.enqueue sequencer_table ~key:sequencer (fun None -> run ())
  ;;

  let post_form ?sequence t uri ~params =
    let (module Cohttp_client_wrapper) = t.cohttp_client_wrapper in
    let headers = Cohttp.Header.init () in
    handle_request ?sequence t ~headers ~f:(fun headers ->
        Cohttp_client_wrapper.post_form ~headers ~params uri
        |> Deferred.Result.map_error ~f:(fun exn -> Error.Endpoint_error exn))
  ;;

  let get ?sequence t uri =
    let (module Cohttp_client_wrapper) = t.cohttp_client_wrapper in
    let headers = Cohttp.Header.init () in
    handle_request ?sequence t ~headers ~f:(fun headers ->
        Cohttp_client_wrapper.get ~headers uri
        |> Deferred.Result.map_error ~f:(fun exn -> Error.Endpoint_error exn))
  ;;

  let set_access_token t ~token ~expiration =
    t.auth.access_token <- No_outstanding_request (Some { token; expiration })
  ;;
end

type t = T : (module T with type t = 't) * 't -> t

let sexp_of_t (T ((module T), t)) = T.sexp_of_t t

let all_rate_limiters =
  Rate_limiter_state_machine.combine
    [ Rate_limiter_state_machine.by_headers
    ; Rate_limiter_state_machine.with_minimum_delay ~delay:(Time_ns.Span.of_int_ms 100)
    ]
;;

let create credentials ~user_agent =
  let rate_limiter =
    Rate_limiter.of_state_machine all_rate_limiters (Time_source.wall_clock ())
  in
  T ((module Local), Local.create credentials ~user_agent ~rate_limiter)
;;

let get ?sequence (T ((module T), t)) = T.get ?sequence t
let post_form ?sequence (T ((module T), t)) = T.post_form ?sequence t

let call_raw t ({ request; sequencer = sequence; handle_response = _ } : _ Endpoint.t) =
  match%bind
    match request with
    | Get { uri } -> get ?sequence t uri
    | Post_form { uri; params } -> post_form ?sequence t uri ~params
  with
  | Ok (response, body) ->
    let%bind body = Cohttp_async.Body.to_string body >>| Cohttp.Body.of_string in
    return (Ok (response, body))
  | Error _ as error -> return error
;;

let call t api =
  match%bind call_raw t api with
  | Error (Access_token_request_error _) as error -> return error
  | Error (Endpoint_error exn) ->
    return (Error (Error.Endpoint_error (Endpoint.Error.Cohttp_raised exn)))
  | Ok (response, body) ->
    api.handle_response (response, body)
    |> Result.map_error ~f:(fun error -> Error.Endpoint_error error)
    |> return
;;

let call_exn t api =
  match%bind call t api with
  | Ok v -> return v
  | Error error -> raise_s ([%sexp_of: Endpoint.Error.t Error.t] error)
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

    module Cohttp_body = struct
      module T = struct
        include Cohttp.Body
        module Binable = Sexp

        let to_binable = Cohttp.Body.sexp_of_t
        let of_binable = Cohttp.Body.t_of_sexp

        let caller_identity =
          Bin_prot.Shape.Uuid.of_string "be6b83a0-fdc6-11eb-9af8-b33c88c1c920"
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
        module Binable = Core.Error

        let to_binable t = Binable.of_exn ~backtrace:`Get t
        let of_binable = Binable.to_exn

        let caller_identity =
          Bin_prot.Shape.Uuid.of_string "dffaba84-e410-11ea-ad49-0755ab1141a3"
        ;;
      end

      include T
      include Bin_prot.Utils.Make_binable_with_uuid (T)
    end

    module Access_token_request_error = struct
      type t = Access_token_request_error.t =
        | Cohttp_raised of Exn.t
        | Json_parsing_error of
            { error : Core.Error.t
            ; response : Cohttp_response.t
            ; body : Cohttp_body.t
            }
        | Token_request_rejected of
            { response : Cohttp_response.t
            ; body : Cohttp_body.t
            }
        | Other_http_error of
            { response : Cohttp_response.t
            ; body : Cohttp_body.t
            }
      [@@deriving sexp_of, bin_io]
    end

    module Error = struct
      type 'endpoint_error t = 'endpoint_error Error.t =
        | Access_token_request_error of Access_token_request_error.t
        | Endpoint_error of 'endpoint_error
      [@@deriving sexp_of, bin_io]
    end

    let get =
      Rpc.Rpc.create
        ~name:"get"
        ~version:2
        ~bin_query:[%bin_type_class: Endpoint.Sequencer.t option * Uri.t]
        ~bin_response:
          [%bin_type_class: (Cohttp_response.t * string, Exn.t Error.t) Result.t]
    ;;

    let post_form =
      Rpc.Rpc.create
        ~name:"post_form"
        ~version:2
        ~bin_query:
          [%bin_type_class:
            Endpoint.Sequencer.t option * Uri.t * (string * string list) list]
        ~bin_response:
          [%bin_type_class: (Cohttp_response.t * string, Exn.t Error.t) Result.t]
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

    let set_access_token _ ~token:_ ~expiration:_ = failwith "Unimplemented"
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
    type t = string Hashtbl.M(String).t

    let create () = Hashtbl.create (module String)

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
      val time_source : Time_source.t
    end

    module Interaction = struct
      let map_headers headers ~f = Cohttp.Header.map (fun _key value -> f value) headers

      module Request = struct
        module T = struct
          type t =
            | Get of
                { uri : Uri_with_string_sexp.t
                ; headers : Cohttp.Header.t
                }
            | Post_form of
                { uri : Uri_with_string_sexp.t
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
          let%bind response = Cohttp_client_wrapper.get uri ~headers >>| Result.ok_exn in
          let%bind response = save_interaction (Get { uri; headers }) response in
          return (Ok response)
        ;;

        let post_form uri ~headers ~params =
          let%bind response =
            Cohttp_client_wrapper.post_form uri ~headers ~params >>| Result.ok_exn
          in
          let%bind response =
            save_interaction (Post_form { uri; headers; params }) response
          in
          return (Ok response)
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
                let json = Jsonaf.of_string body in
                (match Jsonaf.member "access_token" json with
                | None -> ()
                | Some json ->
                  let token = Jsonaf.string_exn json in
                  Placeholders.add placeholders ~secret:token ~placeholder:"access_token"));
              Interaction.map interaction ~f:(Placeholders.filter_string placeholders)
              |> Interaction.sexp_of_t
              |> Sexp.output_mach Out_channel.stdout);
          printf "\n\nPlease move the above to test/%s" filename
        ;;

        let time_source = Time_source.wall_clock ()
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

        let headers_equal =
          Comparable.lift
            [%compare: (String.Caseless.t * string) list]
            ~f:Cohttp.Header.to_list
          |> Base.Comparable.equal
        ;;

        let get uri ~headers =
          let request, response = dequeue_response () in
          let fail () =
            raise_s
              [%message
                "Test request did not match record"
                  (uri : Uri_with_string_sexp.t)
                  (headers : Cohttp.Header.t)
                  ~recorded_request:(request : Interaction.Request.t)]
          in
          match request with
          | Post_form _ -> fail ()
          | Get request ->
            (match Uri.equal uri request.uri && headers_equal headers request.headers with
            | false -> fail ()
            | true -> return (Ok response))
        ;;

        let post_form uri ~headers ~params =
          let request, response = dequeue_response () in
          let fail () =
            raise_s
              [%message
                "Test request did not match record"
                  (uri : Uri_with_string_sexp.t)
                  (headers : Cohttp.Header.t)
                  (params : (string * string list) list)
                  ~recorded_request:(request : Interaction.Request.t)]
          in
          match request with
          | Get _ -> fail ()
          | Post_form request ->
            (match
               Uri.equal uri request.uri
               && headers_equal headers request.headers
               && [%equal: (string * string list) list] params request.params
             with
            | false -> fail ()
            | true -> return (Ok response))
        ;;

        let seal () = assert (Queue.is_empty queue)

        let time_source =
          Time_source.read_only
            (Time_source.create ~now:Time_ns.max_value_representable ())
        ;;
      end)
    ;;

    let with_t filename ~credentials ~f =
      let placeholders = Placeholders.create () in
      (match (credentials : Credentials.t) with
      | Password { client_id; client_secret; username; password } ->
        Placeholders.add placeholders ~secret:client_id ~placeholder:"client_id";
        Placeholders.add placeholders ~secret:client_secret ~placeholder:"client_secret";
        Placeholders.add placeholders ~secret:password ~placeholder:"password";
        Placeholders.add placeholders ~secret:username ~placeholder:"username"
      | Refresh_token { client_id; client_secret; refresh_token } ->
        Placeholders.add placeholders ~secret:client_id ~placeholder:"client_id";
        Option.iter client_secret ~f:(fun secret ->
            Placeholders.add placeholders ~secret ~placeholder:"client_secret");
        Placeholders.add placeholders ~secret:refresh_token ~placeholder:"refresh_token"
      | Userless_confidential { client_id; client_secret } ->
        Placeholders.add placeholders ~secret:client_id ~placeholder:"client_id";
        Placeholders.add placeholders ~secret:client_secret ~placeholder:"client_secret"
      | Userless_public ({ client_id; device_id = _ } as public_credentials) ->
        Placeholders.add placeholders ~secret:client_id ~placeholder:"client_id";
        Placeholders.add
          placeholders
          ~secret:(Credentials.Userless_public.device_id_or_default public_credentials)
          ~placeholder:"device_id");
      Placeholders.add
        placeholders
        ~secret:(Credentials.basic_auth_string credentials)
        ~placeholder:"authorization";
      let%bind file_exists = Sys.file_exists_exn filename in
      let (module Cassette) =
        match file_exists with
        | true -> reading filename placeholders
        | false -> recording filename placeholders
      in
      let rate_limiter =
        Rate_limiter.of_state_machine all_rate_limiters Cassette.time_source
      in
      let connection =
        T
          ( (module Local)
          , Local.create_internal
              (module Cassette)
              credentials
              ~time_source:Cassette.time_source
              ~rate_limiter )
      in
      Monitor.protect
        (fun () -> f connection)
        ~finally:(fun () ->
          Cassette.seal ();
          return ())
    ;;
  end

  let with_cassette filename ~credentials ~f = Cassette.with_t filename ~credentials ~f

  let set_access_token (T ((module T), t)) ~token ~expiration =
    T.set_access_token t ~token ~expiration
  ;;
end
