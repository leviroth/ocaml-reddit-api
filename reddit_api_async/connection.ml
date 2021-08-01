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
      let open Deferred.Result.Let_syntax in
      let%bind _response, body =
        let uri = Uri.of_string "https://www.reddit.com/api/v1/access_token" in
        let headers = Credentials.auth_header t.credentials in
        let params =
          match t.credentials with
          | Password { username; password; _ } ->
            [ "grant_type", [ "password" ]
            ; "username", [ username ]
            ; "password", [ password ]
            ]
          | Refresh_token { refresh_token; _ } ->
            [ "grant_type", [ "refresh_token" ]; "refresh_token", [ refresh_token ] ]
          | Userless_confidential _ -> [ "grant_type", [ "client_credentials" ] ]
          | Userless_public public_credentials ->
            [ "grant_type", [ "https://oauth.reddit.com/grants/installed_client" ]
            ; ( "device_id"
              , [ Credentials.Userless_public.device_id_or_default public_credentials ] )
            ]
        in
        Cohttp_client_wrapper.post_form ~headers uri ~params
      in
      let%bind response_string =
        Monitor.try_with (fun () -> Cohttp_async.Body.to_string body)
      in
      let response_json = Json.of_string response_string in
      let access_token : Access_token.t =
        let token = Json.find response_json [ "access_token" ] |> Json.get_string in
        let expiration =
          let additional_seconds =
            Json.find response_json [ "expires_in" ]
            |> Json.get_float
            |> Time_ns.Span.of_sec
          in
          Time_ns.add (Time_source.now time_source) additional_seconds
        in
        { token; expiration }
      in
      t.access_token <- Some access_token;
      return access_token
    ;;

    let add_access_token t ~headers ~cohttp_client_wrapper ~time_source =
      let open Deferred.Result.Let_syntax in
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
      return (Cohttp.Header.add headers "Authorization" (sprintf "bearer %s" token))
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

  let handle_request
      ?sequence
      { auth; rate_limiter; cohttp_client_wrapper; time_source; sequencer_table }
      ~f
      ~headers
    =
    let open Deferred.Result.Let_syntax in
    let%bind headers =
      Auth.add_access_token auth ~headers ~cohttp_client_wrapper ~time_source
    in
    let run (None : Nothing.t option) =
      let%bind () = Deferred.ok (Rate_limiter.permit_request rate_limiter) in
      let%bind ((response, _body) as result) = f headers in
      Rate_limiter.notify_response rate_limiter response;
      return result
    in
    match sequence with
    | None -> run None
    | Some sequencer -> Sequencer_table.enqueue sequencer_table ~key:sequencer run
  ;;

  let post_form ?sequence t uri ~params =
    let (module Cohttp_client_wrapper) = t.cohttp_client_wrapper in
    let headers = Cohttp.Header.init () in
    handle_request ?sequence t ~headers ~f:(fun headers ->
        Cohttp_client_wrapper.post_form ~headers ~params uri)
  ;;

  let get ?sequence t uri =
    let (module Cohttp_client_wrapper) = t.cohttp_client_wrapper in
    let headers = Cohttp.Header.init () in
    handle_request ?sequence t ~headers ~f:(fun headers ->
        Cohttp_client_wrapper.get ~headers uri)
  ;;
end

type t = T : (module T with type t = 't) * 't -> t

let sexp_of_t (T ((module T), t)) = T.sexp_of_t t

let all_rate_limiters ~time_source =
  Rate_limiter.combine
    [ Rate_limiter.by_headers ~time_source
    ; Rate_limiter.with_minimum_delay ~delay:(Time_ns.Span.of_int_ms 100) ~time_source
    ]
;;

let create credentials ~user_agent =
  T
    ( (module Local)
    , Local.create
        credentials
        ~user_agent
        ~rate_limiter:(all_rate_limiters ~time_source:(Time_source.wall_clock ())) )
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
  | Error error -> raise_s ([%sexp_of: Api.Api_error.t] error)
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
      val time_source : Time_source.t
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
                let json = Json.of_string body in
                let token = Json.find json [ "access_token" ] |> Json.get_string in
                Placeholders.add placeholders ~secret:token ~placeholder:"access_token");
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
            | true -> return (Ok response))
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
      let connection =
        T
          ( (module Local)
          , Local.create_internal
              (module Cassette)
              credentials
              ~time_source:Cassette.time_source
              ~rate_limiter:(all_rate_limiters ~time_source:Cassette.time_source) )
      in
      Monitor.protect
        (fun () -> f connection)
        ~finally:(fun () ->
          Cassette.seal ();
          return ())
    ;;
  end

  let with_cassette filename ~credentials ~f = Cassette.with_t filename ~credentials ~f
end
