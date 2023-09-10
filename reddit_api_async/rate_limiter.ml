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
          let time = Time_ns_unix.of_string time in
          print_s [%sexp (snap_to_nearest_minute time : Time_ns.Alternate_sexp.t)]);
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
        Some (snap_to_nearest_minute (Time_ns.add server_time relative_reset_time))
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
  end

  type t =
    { ready : (unit, read_write) Mvar.t
    ; time_source : (Time_source.t[@sexp.opaque])
    ; mutable reset_event : ((Nothing.t, unit) Time_source.Event.t[@sexp.opaque]) option
    ; mutable server_side_info : Server_side_info.t option
    }
  [@@deriving sexp_of]

  let create ~time_source =
    let ready = Mvar.create () in
    Mvar.set ready ();
    { server_side_info = None; reset_event = None; time_source; ready }
  ;;

  let is_ready { ready; _ } = not (Mvar.is_empty ready)
  let wait_until_ready { ready; _ } = Mvar.value_available ready

  let rec schedule_reset_at_time t time =
    let schedule_fresh_event () =
      t.reset_event
        <- Some
             (Time_source.Event.run_at
                t.time_source
                time
                (fun () ->
                  Mvar.set t.ready ();
                  (* In case something prevents our first request from receiving
                     a response, we will periodically allow retries. *)
                  schedule_reset_at_time t (Time_ns.add time Time_ns.Span.minute))
                ())
    in
    match t.reset_event with
    | None -> schedule_fresh_event ()
    | Some event ->
      let scheduled_time = Time_source.Event.scheduled_at event in
      (match Time_ns.( < ) scheduled_time time with
      | false -> ()
      | true ->
        (match Time_source.Event.reschedule_at event time with
        | Ok -> ()
        | Previously_aborted _ -> .
        | Previously_happened () -> schedule_fresh_event ()))
  ;;

  let update_server_side_info t ~new_server_side_info =
    t.server_side_info <- Some new_server_side_info;
    schedule_reset_at_time t new_server_side_info.reset_time;
    match
      ( new_server_side_info.remaining_api_calls > 0
      , Time_ns.equal new_server_side_info.reset_time Time_ns.epoch )
    with
    | false, false -> ()
    | false, true ->
      [%log.debug
        Import.log
          "Rate limit exhausted"
          ~reset_time:(new_server_side_info.reset_time : Time_ns_unix.t)]
    | true, _ -> Mvar.set t.ready ()
  ;;

  let permit_request t =
    let%bind () = Mvar.take t.ready in
    (match t.server_side_info with
    | None -> ()
    | Some ({ remaining_api_calls; _ } as server_side_info) ->
      (match remaining_api_calls with
      | 0 -> ()
      | n ->
        let new_server_side_info =
          { server_side_info with remaining_api_calls = n - 1 }
        in
        update_server_side_info t ~new_server_side_info));
    return ()
  ;;

  let notify_response t response =
    let headers = Cohttp.Response.headers response in
    match Server_side_info.t_of_headers headers with
    | None ->
      (* We assume that, in the absence of ratelimit headers, we must have hit
         some authentication failure. As a heuristic to avoid getting stuck, we
         immediately reset [t.ready]. *)
      Mvar.set t.ready ()
    | Some response_server_side_info ->
      let new_server_side_info =
        match t.server_side_info with
        | None -> response_server_side_info
        | Some server_side_info ->
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
  type nonrec t = t list [@@deriving sexp_of]

  let kind = "Combined"
  let permit_request ts = Deferred.all_unit (List.map ts ~f:permit_request)
  let notify_response ts headers = List.iter ts ~f:(fun t -> notify_response t headers)
  let is_ready ts = List.for_all ts ~f:is_ready
  let wait_until_ready ts = Deferred.all_unit (List.map ts ~f:wait_until_ready)
end

let combine ts = T ((module Combined), ts)
