open! Core

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
      (parse_http_header_date "Wed, 21 Oct 2015 07:28:00 GMT" : Time_ns.Alternate_sexp.t)];
  [%expect {| "2015-10-21 07:28:00Z" |}]
;;
