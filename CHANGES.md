# Unreleased

## Added

- Add various user overview endpoints.
- Add `Mod_action.compare` and `sexp_of`.
- Add `Mod_action.target_author`.
- Add `sexp_of` to `Json_object` interfaces.

## Changed

- Use `jsonaf` instead of `ezjsonm`.
- Add explicit interface to `reddit_api_kernel`
  - Expose `foo_intf.ml` contents as `Reddit_api_kernel.Foo`.
- Expose a library-level `Log.t`, `Reddit_api_async.Logging.log`.
  - All logging goes through this value instead of `Log.Global`.
  - The `Log.t` initially has no output, but can have its output set by the
    application.
- Log when `Retry_manager` retries.
- Rename `Non_transient_error` to `Permanent_error`.
- Rename `Json_object.Util.time` => `time_sec_since_epoch`.
- Require Jane Street libraries >= v0.16.0.
- Make `Stream` functions require a `Hashtbl.Key_plain` module instead of the
  larger `Hashable.S`.
- Fix a bug with `Mod_action.moderator`.
- Change `Stream` to provide one function, `stream`, that returns a pipe,
  instead of several functions with different interfaces.
- Change `Iter_comments` to return a pipe.
- Fix a crash when replying to messages by introducing the new
  `Endpoint.reply_to_message`.
- Revamp rate limiter. The rate limiter is now built on top of a synchronous
  state machine with a small async wrapper. We eliminate various async
  background jobs.

## Removed

- Remove `[@@deriving bin_io]` in `Thing` interfaces.

# 0.2.1

## Removed

- A comment in the `reddit_api_kernel` opam file.

# 0.2.0

## Added

- Add `Endpoint.user_trophies` to view trophies by user.
- Add `Thing.Link.Contents.t` variant for distinguishing self and link
  submissions.
- Add `Thing.Comment.link_title` to view the title of the link for a comment in
  the modqueue.
- Add support for OAuth2 userless (app-only) flow. Both public clients and
  confidential clients are supported. See `Connection.Credentials.t`.
- Add support for OAuth2 refresh token credentials.
- Add handling of failed access token requests.
- Add automatic retries when an endpoint request fails due to an expired access
  token.

## Changed

- Require dune >= 2.8.
- Require cohttp >= 5.0.0.
- Add a case to `Endpoint.Error` representing Reddit's JSON error responses.
- Change `Connection.Credentials.t` from record to variant that supports
  OAuth2 userless (app-only) flow, so if you serialize the variant,
  a constructor name will be prepended to the sexp.
- Restructure `Rate_limiter` and added expect tests.
- Reorganize error types in `Connection` and `Retry_manager` to distinguish
  errors in fetching an access token.
- Bump `Connection.Remote` protocol to version 2.
- Rename `Api` to `Endpoint` and `Api.Api_error` to `Api.Error`.
- Remove `Endpoint.with_param_override` type.
  - This changes the type of each endpoint value.
  - HTTP request parameters can instead be tweaked by simply editing the
    `Endpoint.request` field.

# 0.1.1 (2020-12-30)

## Added

- Add lots of documentation.

## Changed

- Increase the minimum delay between HTTP requests from 10ms to 100ms.
- Return a special `Inbox_item.Comment.t` value instead of a `Thing.Comment.t`
  for inbox endpoints.

## Removed

- Remove unused optional `subreddit` parameter from `Api.info`.
- Remove `Bounded_set` from the public interface of `reddit_api_async`.
