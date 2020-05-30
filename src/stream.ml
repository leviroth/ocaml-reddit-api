open! Core
open! Async

module Backoff : sig
  type t

  val initial : t
  val increment : t -> t
  val after : t -> unit Deferred.t
end = struct
  type t = Time_ns.Span.t

  let initial = Time_ns.Span.second
  let max_val = Time_ns.Span.of_int_sec 16
  let increment t = Time_ns.Span.min max_val (Time_ns.Span.scale t 2.)
  let after = Clock_ns.after
end

let iter
    (type id)
    (module Id : Hashable.S with type t = id)
    connection
    ~get_listing
    ~get_before_parameter
    ~f
  =
  let module Bounded_set = Bounded_set.Make (Id) in
  let seen = Bounded_set.create ~capacity:300 in
  let rec loop ?(first_pass = false) before backoff cache_busting_counter =
    let limit, cache_busting_counter =
      match before with
      | Some _ -> 100, cache_busting_counter
      | None ->
        let cache_busting_counter = (cache_busting_counter + 1) mod 30 in
        100 - cache_busting_counter, cache_busting_counter
    in
    match%bind get_listing connection ~before ~limit with
    | Error (response, (_ : Cohttp_async.Body.t)) ->
      let status = Cohttp.Response.status response in
      let code = Cohttp.Code.code_of_status status in
      Log.Global.error_s
        [%message
          "Received error response" (code : int) (status : Cohttp.Code.status_code)];
      let backoff = Backoff.increment backoff in
      let%bind () = Backoff.after backoff in
      loop before backoff cache_busting_counter
    | Ok list ->
      let list =
        List.rev list
        |> List.filter ~f:(fun child ->
               not (Bounded_set.mem seen (get_before_parameter child : id)))
      in
      let%bind () =
        Deferred.List.iter list ~f:(fun child ->
            Bounded_set.add seen (get_before_parameter child);
            match first_pass with
            | true -> return ()
            | false -> f child)
      in
      (match List.hd list with
      | None ->
        let backoff = Backoff.increment backoff in
        let%bind () = Backoff.after backoff in
        loop None backoff cache_busting_counter
      | Some child ->
        let before = Some (get_before_parameter child) in
        let backoff = Backoff.initial in
        let%bind () = Backoff.after backoff in
        loop before backoff cache_busting_counter)
  in
  loop ~first_pass:true None Backoff.initial 0
;;
