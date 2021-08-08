(** [Retry_manager] handles transient errors due to blips in networking or
    Reddit's infrastructure.

    When a request is sent via a retry manager, a transient error causes the
    manager to periodically query the Reddit API to detect a resumption of
    ordinary service. After this query succeeds, the original request is
    retried.

    As a result, the return type of {!val:Retry_manager.call} does not include
    transient errors.

    {b Warning.} Do not use [Retry_manager] if it is critical that you do not
    perform the same action twice. Reddit has been known to send HTTP server
    error statuses even while successfully handling the request. Therefore,
    [Retry_manager] cannot guarantee that a request had no effect before
    retrying it - it just trusts Reddit when it says that there was an error.

    {1 Transient and non-transient errors}

    A transient error is an error that we expect to resolve without changes to
    the API parameters. We operationalize this as

    - any exception raised by the [Cohttp] client module; or
    - any HTTP response with a server error status code (500-599).

    {b Example: Transient error.} Reddit responds to a request with [503
    Service Unavailable].  We expect that service will eventually be restored,
    and the same request will then succeed. This is a transient error.

    {b Example: Non-transient error.} Reddit responds to a request with [403
    Forbidden]. We expect that the request will not succeed unless either (a)
    the request is modified to no longer reference content to which the user
    does not have access; or (b) the user's permissions are modified outside of
    this request.
*)

open! Core
open! Async
open Reddit_api_kernel

type t

val create : Connection.t -> t

module Non_transient_error : sig
  type t =
    | Http_error of
        { response : Cohttp.Response.t
        ; body : Cohttp.Body.t
        }
    | Json_response_errors of Api.Json_response_error.t list
  [@@deriving sexp_of]
end

(** [call t f] immediately calls [f] unless the last result of such a call
    was a transient error. In the latter case, all calls block, and [call]
    periodically calls a read-only API endpoint until service is restored.
*)
val call : t -> 'a Api.t -> ('a, Non_transient_error.t) Deferred.Result.t

(** [yield_until_reddit_available] returns immediately if there is no known
    transient error; it never causes an HTTP request. *)
val yield_until_reddit_available : t -> unit Deferred.t
