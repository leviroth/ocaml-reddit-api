open! Core
open! Async

module type S = sig
  type t [@@deriving sexp_of]

  val kind : string
  val permit_request : t -> unit Deferred.t
  val notify_response : t -> Cohttp.Response.t -> unit
  val is_ready : t -> bool
  val wait_until_ready : t -> unit Deferred.t
end

type t = T : (module S with type t = 't) * 't -> t

let sexp_of_t (T ((module S), t)) : Sexp.t = List [ Atom S.kind; [%sexp_of: S.t] t ]
let permit_request (T ((module S), t)) = S.permit_request t
let notify_response (T ((module S), t)) = S.notify_response t
let is_ready (T ((module S), t)) = S.is_ready t
let wait_until_ready (T ((module S), t)) = S.wait_until_ready t

module With_minimum_delay = struct
  type t =
    { wait_until : (Time_ns.Alternate_sexp.t option, read_write) Mvar.t
    ; time_source : (Time_source.t[@sexp.opaque])
    ; delay : Time_ns.Span.t
    }
  [@@deriving sexp_of]

  let kind = "With_minimum_delay"

  let create ~delay ~time_source =
    let wait_until = Mvar.create () in
    Mvar.set wait_until None;
    { wait_until; delay; time_source }
  ;;

  let permit_request { wait_until; delay; time_source } =
    let%bind () =
      match%bind Mvar.take wait_until with
      | None -> return ()
      | Some wait_until -> Time_source.at time_source wait_until
    in
    Mvar.set wait_until (Some (Time_ns.add (Time_source.now time_source) delay));
    return ()
  ;;

  let notify_response (_ : t) (_ : Cohttp.Response.t) = ()

  let is_ready { wait_until; time_source; _ } =
    match Mvar.peek wait_until with
    | None -> false
    | Some None -> true
    | Some (Some wait_until) -> Time_ns.( >= ) (Time_source.now time_source) wait_until
  ;;

  let wait_until_ready { wait_until; time_source; _ } =
    let%bind mvar_contents = Mvar.take wait_until in
    let%bind () =
      match mvar_contents with
      | None -> return ()
      | Some wait_until -> Time_source.at time_source wait_until
    in
    Mvar.set wait_until mvar_contents;
    return ()
  ;;
end

module By_headers = struct
  let kind = "By_headers"

  module Server_side_info = struct
    type t =
      { remaining_api_calls : int
      ; reset_time : Time_ns_unix.t
      }
    [@@deriving sexp, fields]

    let snap_to_nearest interval time =
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
          let time = Time_ns_unix.of_string time in
          print_s
            [%sexp (snap_to_nearest Time_ns.Span.minute time : Time_ns.Alternate_sexp.t)]);
      [%expect {|
          "2020-11-30 18:48:00Z"
          "2020-11-30 18:48:00Z" |}];
      return ()
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
          Time_ns.of_date_ofday date ofday ~zone:Time_ns_unix.Zone.utc)
    ;;

    let%expect_test _ =
      print_s
        [%sexp
          (parse_http_header_date "Wed, 21 Oct 2015 07:28:00 GMT"
            : Time_ns.Alternate_sexp.t)];
      [%expect {| "2015-10-21 07:28:00Z" |}];
      return ()
    ;;

    let t_of_headers headers =
      let open Option.Let_syntax in
      let get_header header = Cohttp.Header.get headers header in
      let%bind remaining_api_calls =
        get_header "X-Ratelimit-Remaining" >>| Float.of_string >>| Int.of_float
      in
      let%bind reset_time =
        let%bind server_time = get_header "Date" >>| parse_http_header_date in
        let%bind relative_reset_time =
          get_header "X-Ratelimit-Reset" >>| Int.of_string >>| Time_ns.Span.of_int_sec
        in
        Some
          (snap_to_nearest
             Time_ns.Span.minute
             (Time_ns.add server_time relative_reset_time))
      in
      Some { remaining_api_calls; reset_time }
    ;;

    let compare_by_inferred_age =
      (* We use reset time instead of the "Date" header because we might have
         sent some requests for which we have yet to receive a response. In that
         case, the most authoritative picture of our remaining requests is not
         the most recent response, but rather our previous state adjusted by the
         number of requests we've sent.

         We therefore prefer the state with fewer requests remaining when two
         states are for the same reset period.
      *)
      Comparable.lexicographic
        [ Comparable.lift Time_ns.compare ~f:reset_time
        ; Comparable.reverse (Comparable.lift compare ~f:remaining_api_calls)
        ]
    ;;

    let freshest = Base.Comparable.max compare_by_inferred_age

    let state_at_start_of_window ~representative_time =
      let reset_time =
        Time_ns.next_multiple
          ~can_equal_after:false
          ~interval:(Time_ns.Span.of_int_min 10)
          ~base:Time_ns.epoch
          ~after:representative_time
          ()
      in
      let remaining_api_calls = 996 in
      { remaining_api_calls; reset_time }
    ;;
  end

  module State = struct
    type t =
      | Created
      | Waiting_on_first_request
      | Consuming_rate_limit of Server_side_info.t
    [@@deriving sexp_of]
  end

  type t =
    { mutable state : State.t
    ; updated : (unit, read_write) Bvar.t
    ; time_source : (Time_source.t[@sexp.opaque])
    }
  [@@deriving sexp_of]

  let create ~time_source =
    let updated = Bvar.create () in
    { time_source; state = Created; updated }
  ;;

  let is_ready { state; time_source; _ } =
    match state with
    | Created -> true
    | Waiting_on_first_request -> false
    | Consuming_rate_limit { remaining_api_calls; reset_time } ->
      remaining_api_calls > 0 || Time_ns.( >= ) (Time_source.now time_source) reset_time
  ;;

  let wait_until_ready t =
    Deferred.repeat_until_finished () (fun () ->
        match t.state with
        | Created -> return (`Finished ())
        | Waiting_on_first_request ->
          let%bind () = Bvar.wait t.updated in
          return (`Repeat ())
        | Consuming_rate_limit { remaining_api_calls; reset_time } ->
          let now = Time_source.now t.time_source in
          (* TODO: Move above? *)
          (match Time_ns.( >= ) now reset_time with
          | true ->
            t.state
              <- Consuming_rate_limit
                   (Server_side_info.state_at_start_of_window ~representative_time:now);
            return (`Finished ())
          | false ->
            (match remaining_api_calls > 0 with
            | true -> return (`Finished ())
            | false ->
              let%bind () = Time_source.at t.time_source reset_time in
              return (`Repeat ()))))
  ;;

  (* TODO: Allow retries once per minute? *)

  let update_server_side_info t ~new_server_side_info =
    t.state <- Consuming_rate_limit new_server_side_info;
    Bvar.broadcast t.updated ();
    match
      ( new_server_side_info.remaining_api_calls > 0
      , Time_ns.equal new_server_side_info.reset_time Time_ns.epoch )
    with
    | false, false | true, (true | false) -> ()
    | false, true ->
      [%log.debug
        Import.log
          "Rate limit exhausted"
          ~reset_time:(new_server_side_info.reset_time : Time_ns_unix.t)]
  ;;

  let permit_request t =
    let%bind () = wait_until_ready t in
    match t.state with
    | Waiting_on_first_request -> (* TODO is this impossible? *) assert false
    | Created ->
      t.state <- Waiting_on_first_request;
      return ()
    | Consuming_rate_limit ({ remaining_api_calls; _ } as server_side_info) ->
      let new_server_side_info : Server_side_info.t =
        match remaining_api_calls with
        | 0 -> server_side_info
        | n -> { server_side_info with remaining_api_calls = n - 1 }
      in
      update_server_side_info t ~new_server_side_info;
      return ()
  ;;

  let notify_response t response =
    let headers = Cohttp.Response.headers response in
    match Server_side_info.t_of_headers headers with
    | None ->
      (* We assume that, in the absence of ratelimit headers, we must have hit
         some authentication failure. As a heuristic to avoid getting stuck, we
         immediately reset [t.ready]. *)
      (* TODO: What to do here? *)
      t.state <- Created
    | Some response_server_side_info ->
      let new_server_side_info =
        match t.state with
        | Created -> raise_s [%message "[notify_response] called before [permit_request]"]
        | Waiting_on_first_request -> response_server_side_info
        | Consuming_rate_limit server_side_info ->
          (match
             Comparable.lift
               [%compare: Time_ns.t]
               ~f:Server_side_info.reset_time
               server_side_info
               response_server_side_info
             |> Ordering.of_int
           with
          | Greater | Equal -> ()
          | Less ->
            [%log.debug
              Import.log
                "Rate limit is resetting"
                ~old_remaining_api_calls:(server_side_info.remaining_api_calls : int)]);
          Server_side_info.freshest server_side_info response_server_side_info
      in
      update_server_side_info t ~new_server_side_info
  ;;
end

let by_headers ~time_source = T ((module By_headers), By_headers.create ~time_source)

let with_minimum_delay ~time_source ~delay =
  T ((module With_minimum_delay), With_minimum_delay.create ~time_source ~delay)
;;

module Combined = struct
  type nonrec t =
    { ts : t list
    ; sequencer : unit Throttle.Sequencer.t
    }
  [@@deriving sexp_of]

  let kind = "Combined"

  let create ts =
    let sequencer = Throttle.Sequencer.create () in
    { ts; sequencer }
  ;;

  let permit_request { ts; sequencer } =
    Throttle.enqueue sequencer (fun () ->
        Deferred.all_unit (List.map ts ~f:permit_request))
  ;;

  let notify_response { ts; _ } headers =
    List.iter ts ~f:(fun t -> notify_response t headers)
  ;;

  let is_ready { ts; _ } = List.for_all ts ~f:is_ready

  let wait_until_ready { ts; sequencer } =
    Throttle.enqueue sequencer (fun () ->
        Deferred.all_unit (List.map ts ~f:wait_until_ready))
  ;;
end

let combine ts = T ((module Combined), Combined.create ts)
