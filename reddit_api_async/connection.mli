(** [Connection] manages the connection to Reddit, including authentication and
    rate limiting behavior.

    It is responsible for taking the endpoint specifications in
    {!module:Reddit_api_kernel.Endpoint} and actually performing HTTP requests.

    Consider wrapping your [Connection] in a {!module:Retry_manager} if you are
    writing a long-running process and want to just retry forever on transient
    errors.

    {1 Authentication }

    [Connection] currently supports a subset of Reddit's OAuth2 app types via
    the {!module:Credentials} module. See
    {{:https://github.com/reddit-archive/reddit/wiki/oauth2-app-types} Reddit's
    documentation on app types}.

    {1 Rate-limiting behavior }

    [Connection] enforces two different forms of rate-limiting:

    {2 HTTP rate-limiting headers }

    Reddit tracks API usage and requires that a client make no more than 600
    requests in a 10 minute period.

    Rather than simply keeping a counter of requests internally, [Connection]
    reads Reddit's API response headers to learn about the request quota,
    including the number of remaining requests and the time until the quota
    resets. This allows multiple [Connection.t]s with the same credentials to
    run in parallel, accounting for each others' quota usage without explicit
    coordination.

    {2 Minimum time between requests }

    In order to abide by /u/kemitche's
    {{:https://www.reddit.com/r/redditdev/comments/1yxrp7/formal_ratelimiting_headers/}
    request} to "be reasonable" and not slam all 600 requests in as quickly as
    possible, [Connection] also enforces a 100ms delay between requests.
*)

open! Core
open! Async
open Reddit_api_kernel

module Credentials : sig
  (** [Password] credentials correspond to Reddit's
      {{:https://github.com/reddit-archive/reddit/wiki/oauth2-app-types#script}"script"}
      app type.

      @see < https://datatracker.ietf.org/doc/html/rfc6749#section-4.3.2 > The
      RFC 6749 section describing the corresponding access token request. *)
  module Password : sig
    type t =
      { client_id : string
      ; client_secret : string
      ; password : string
      ; username : string
      }
    [@@deriving sexp]
  end

  (** [Refresh_token] credentials correspond to Reddit's
      {{:https://github.com/reddit-archive/reddit/wiki/oauth2-app-types#web-app}"web app"}
      and
      {{:https://github.com/reddit-archive/reddit/wiki/oauth2-app-types#installed-app}"installed-app"}
      app types.

      @see < https://praw.readthedocs.io/en/stable/tutorials/refresh_token.html
      > {{:https://praw.readthedocs.io/}PRAW}'s documentation on refresh tokens
      for advice on obtaining a refresh token, which is currently outside the
      scope of this project.

      @see < https://datatracker.ietf.org/doc/html/rfc6749#section-6 > The RFC
      6749 section describing the corresponding access token request. *)
  module Refresh_token : sig
    type t =
      { client_id : string
      ; client_secret : string option
            (** This field is present for web apps and absent for installed apps. *)
      ; refresh_token : string
      }
    [@@deriving sexp]
  end

  module Userless_confidential : sig
    type t =
      { client_id : string
      ; client_secret : string
      }
    [@@deriving sexp]
  end

  module Userless_public : sig
    type t =
      { client_id : string
      ; device_id : string option
      }
    [@@deriving sexp]
  end

  type t =
    | Password of Password.t
    | Refresh_token of Refresh_token.t
    | Userless_confidential of Userless_confidential.t
    | Userless_public of Userless_public.t
  [@@deriving sexp]
end

type t [@@deriving sexp_of]

(** An [Access_token_error.t] represents an error encountered while fetching an access
    token. *)
module Access_token_error : sig
  type t =
    | Cohttp_raised of Exn.t
    | Json_parsing_error of
        { error : Error.t
        ; response : Cohttp.Response.t
        ; body_string : string
        }
  [@@deriving sexp_of]
end

(** A request via [Connection] may result in two different HTTP requests: a
    request to the explicitly named API endpoint and, as necessary, a request to
    retrieve an OAuth2 access token. This type distinguishes errors associated
    with each of these requests. *)
module Error : sig
  type 'endpoint_error t =
    | Access_token_error of Access_token_error.t
    | Endpoint_error of 'endpoint_error
  [@@deriving sexp_of]
end

val create : Credentials.t -> user_agent:string -> t
val call : t -> 'a Endpoint.t -> ('a, Endpoint.Error.t Error.t) Result.t Deferred.t
val call_exn : t -> 'a Endpoint.t -> 'a Deferred.t

(** [call_raw] returns the raw HTTP response from Reddit. *)
val call_raw
  :  t
  -> 'a Endpoint.t
  -> (Cohttp.Response.t * Cohttp.Body.t, Exn.t Error.t) Result.t Deferred.t

(** Any connection can be turned into an RPC server, acting as a shared
    connection for multiple client [Connection.t]s. Rate limiting is managed on
    the server side.

    {b Important.} This feature is highly experimental. The protocol may change
    without warning. *)
module Remote : sig
  val serve
    :  t
    -> where_to_listen:(([< Socket.Address.t ] as 'a), 'b) Tcp.Where_to_listen.t
    -> ('a, 'b) Tcp.Server.t Deferred.t

  val connect_exn : [< Socket.Address.t ] Tcp.Where_to_connect.t -> t Deferred.t
end

module For_testing : sig
  val with_cassette
    :  Filename.t
    -> credentials:Credentials.t
    -> f:(t -> 'a Deferred.t)
    -> 'a Deferred.t
end
