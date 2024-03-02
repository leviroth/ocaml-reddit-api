open! Core

module When_to_send = struct
  type t =
    | Now
    | Check_after_receiving_response
    | After of Time_ns.t
end

module type Basic = sig
  type t [@@deriving sexp_of]

  val kind : string
  val wait_until : t -> When_to_send.t
  val sent_request_unchecked : t -> now:Time_ns.t -> t
  val received_response : t -> Cohttp.Response.t -> t
end

type t = T : (module Basic with type t = 't) * 't -> t

let sexp_of_t (T ((module S), t)) : Sexp.t = List [ Atom S.kind; [%sexp_of: S.t] t ]
let wait_until (T ((module S), t)) = S.wait_until t

let sent_request_unchecked (T (((module M) as m), t)) ~now =
  T (m, M.sent_request_unchecked t ~now)
;;

let received_response (T (((module M) as m), t)) response =
  T (m, M.received_response t response)
;;

module With_minimum_delay = struct
  type t =
    { last_request : Time_ns.Alternate_sexp.t option
    ; delay : Time_ns.Span.t
    }
  [@@deriving sexp_of]

  let kind = "With_minimum_delay"
  let create ~delay = { last_request = None; delay }

  let wait_until t : When_to_send.t =
    match t.last_request with
    | None -> Now
    | Some time -> After (Time_ns.add time t.delay)
  ;;

  let sent_request_unchecked t ~now = { t with last_request = Some now }
  let received_response t (_ : Cohttp.Response.t) = t
end

module By_headers = struct
  let kind = "By_headers"

  module Server_side_info = struct
    type t =
      { remaining_api_calls : int
      ; reset_time : Time_ns.Alternate_sexp.t
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
          let time = Time_ns.of_string_with_utc_offset time in
          print_s
            [%sexp (snap_to_nearest Time_ns.Span.minute time : Time_ns.Alternate_sexp.t)]);
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
          Time_ns.of_date_ofday date ofday ~zone:Time_float.Zone.utc)
    ;;

    let%expect_test _ =
      print_s
        [%sexp
          (parse_http_header_date "Wed, 21 Oct 2015 07:28:00 GMT"
            : Time_ns.Alternate_sexp.t)];
      [%expect {| "2015-10-21 07:28:00Z" |}]
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

  type t =
    | Created
    | Waiting_on_first_request
    | Consuming_rate_limit of Server_side_info.t
  [@@deriving sexp_of]

  let wait_until t : When_to_send.t =
    match t with
    | Created -> Now
    | Waiting_on_first_request -> Check_after_receiving_response
    | Consuming_rate_limit { remaining_api_calls; reset_time } ->
      (match remaining_api_calls > 0 with
      | true -> Now
      | false -> After reset_time)
  ;;

  let sent_request_unchecked t ~now =
    match t with
    | Created -> Waiting_on_first_request
    | Waiting_on_first_request ->
      raise_s
        [%message
          "[sent_request_unchecked] illegally called in [Waiting_on_first_request] state."]
    | Consuming_rate_limit server_side_info ->
      let base_server_side_info =
        match Time_ns.( <= ) server_side_info.reset_time now with
        | false -> server_side_info
        | true -> Server_side_info.state_at_start_of_window ~representative_time:now
      in
      Consuming_rate_limit
        { base_server_side_info with
          remaining_api_calls = base_server_side_info.remaining_api_calls - 1
        }
  ;;

  let received_response t response =
    let headers = Cohttp.Response.headers response in
    match Server_side_info.t_of_headers headers with
    | None ->
      (* We assume that, in the absence of ratelimit headers, we must have hit
         some authentication failure. As a heuristic to avoid getting stuck, we
         immediately reset [t.ready]. *)
      Created
    | Some response_server_side_info ->
      (match t with
      | Created ->
        raise_s [%message "[received_response] called before [sent_request_unchecked]."]
      | Waiting_on_first_request -> Consuming_rate_limit response_server_side_info
      | Consuming_rate_limit server_side_info ->
        Consuming_rate_limit
          (Server_side_info.freshest server_side_info response_server_side_info))
  ;;
end

let by_headers = T ((module By_headers), Created)

let with_minimum_delay ~delay =
  T ((module With_minimum_delay), With_minimum_delay.create ~delay)
;;

module Combined = struct
  type nonrec t = t list [@@deriving sexp_of]

  let kind = "Combined"
  let create ts = ts

  let wait_until ts =
    match
      List.map ts ~f:wait_until
      |> List.max_elt ~compare:(fun a b ->
             match a, b with
             | Now, _ -> -1
             | _, Now -> 1
             | After a, After b -> Time_ns.compare a b
             | _, _ -> 0)
    with
    | Some v -> v
    | None -> Now
  ;;

  let sent_request_unchecked ts ~now = List.map ts ~f:(sent_request_unchecked ~now)

  let received_response ts response =
    List.map ts ~f:(fun t -> received_response t response)
  ;;
end

let combine ts = T ((module Combined), Combined.create ts)
