(** [Retry_manager] handles transient errors due to blips in networking or
    Reddit's infrastructure.

    When a [GET] request is sent via a retry manager, a transient error causes the
    manager to periodically query the Reddit API to detect a resumption of
    ordinary service. After this query succeeds, the original request is
    retried.

    [POST] requests are not retried because they are not idempotent. In
    practice, we have observed that Reddit sometimes responds with [500 Internal
    Server Error] to a [POST] request that has actually succeeded in performing
    a side effect (such as leaving a comment). In such a case, retrying would
    lead to us repeatedly leaving the same comment until the server condition
    clears.

    {1 Transient and permanent errors}

    A transient error is an error that we expect to resolve without changes to
    the API parameters. We operationalize this as

    - any exception raised by the [Cohttp] client module; or
    - any HTTP response with a server error status code (500-599).

    The terms "transient" and "permanent" are borrowed from
    {{:https://datatracker.ietf.org/doc/html/rfc5321#section-4.2.1} SMTP reply
    codes}.

    {b Example: Transient error.} Reddit responds to a request with [503
    Service Unavailable].  We expect that service will eventually be restored,
    and the same request will then succeed. This is a transient error.

    {b Example: Permanent error.} Reddit responds to a request with [403
    Forbidden]. We expect that the request will not succeed unless either (a)
    the request is modified to no longer reference content to which the user
    does not have access; or (b) the user's permissions are modified outside of
    this request. *)

open! Core
open! Async
open Reddit_api_kernel

type t

val create : Connection.t -> t

(** [call t f] immediately calls [f] unless the last result of such a call
    was a transient error. In the latter case, all calls block, and [call]
    periodically calls a read-only API endpoint until service is restored. *)
val call
  :  t
  -> 'a Endpoint.t
  -> ('a, Endpoint.Error.t Connection.Error.t) Deferred.Result.t

(** [yield_until_reddit_available] returns immediately if there is no known
    transient error; it never causes an HTTP request. *)
val yield_until_reddit_available : t -> unit Deferred.t
