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

module State = struct
  type 'id t =
    { first_pass : bool
    ; before : 'id option
    ; backoff : Backoff.t
    ; cache_busting_counter : int
    }
end

let stream
    (type id)
    (module Id : Hashtbl.Key_plain with type t = id)
    connection
    ~get_listing
    ~get_before_parameter
  =
  let module Bounded_set = Bounded_set.Make (Id) in
  let seen = Bounded_set.create ~capacity:300 in
  Pipe.create_reader ~close_on_exception:false (fun pipe ->
      Deferred.repeat_until_finished
        { State.first_pass = true
        ; before = None
        ; backoff = Backoff.initial
        ; cache_busting_counter = 0
        }
        (fun ({ first_pass; before; backoff; cache_busting_counter } : Id.t State.t) ->
          let loop_after_backoff_and_pushback (state : _ State.t) =
            let%bind () = Backoff.after state.backoff
            and () = Pipe.pushback pipe in
            return (`Repeat state)
          in
          match Pipe.is_closed pipe with
          | true -> return (`Finished ())
          | false ->
            let limit, cache_busting_counter =
              match before with
              | Some _ -> 100, cache_busting_counter
              | None -> 100 - cache_busting_counter, (cache_busting_counter + 1) mod 30
            in
            (match%bind Connection.call connection (get_listing ~before ~limit) with
            | Error _ as response ->
              Pipe.write_without_pushback_if_open pipe response;
              let backoff = Backoff.increment backoff in
              loop_after_backoff_and_pushback
                { first_pass; before; backoff; cache_busting_counter }
            | Ok list_newest_to_oldest ->
              let list_newest_to_oldest =
                List.filter list_newest_to_oldest ~f:(fun child ->
                    not (Bounded_set.mem seen (get_before_parameter child : id)))
              in
              List.iter list_newest_to_oldest ~f:(fun child ->
                  Bounded_set.add seen (get_before_parameter child));
              (match first_pass with
              | true -> ()
              | false ->
                let list_oldest_to_newest = List.rev list_newest_to_oldest in
                List.iter list_oldest_to_newest ~f:(fun elt ->
                    Pipe.write_without_pushback_if_open pipe (Ok elt)));
              let before =
                let most_recent_element = List.hd list_newest_to_oldest in
                Option.map most_recent_element ~f:get_before_parameter
              in
              let backoff =
                match List.is_empty list_newest_to_oldest with
                | true -> Backoff.increment backoff
                | false -> Backoff.initial
              in
              loop_after_backoff_and_pushback
                { first_pass = false; before; backoff; cache_busting_counter })))
;;
