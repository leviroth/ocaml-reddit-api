open! Core
open! Async

let test connection i =
  let uri = Uri.of_string "https://oauth.reddit.com/api/v1/me" in
  let%bind response, body =
    Ocaml_reddit.Connection.with_t
      connection
      ~headers:(Cohttp.Header.init ())
      ~f:(fun headers -> Cohttp_async.Client.call ~headers `GET uri)
  in
  Log.Global.info_s
    [%message
      "got response"
        (i : int)
        (response : Cohttp.Response.t)
        (body : Cohttp_async.Body.t)
        (connection : Ocaml_reddit.Connection.t)];
  return ()
;;

let command =
  let open Command.Let_syntax in
  let%map_open auth_config_path = anon ("CONFIG" %: string) in
  fun () ->
    Log.Global.set_output
      [ Log.Output.stdout ()
      ; Log.Output.rotating_file `Sexp ~basename:"test/output" (Log.Rotation.default ())
      ];
    let connection =
      let config =
        Sexp.load_sexp_conv_exn
          auth_config_path
          [%of_sexp: Ocaml_reddit.Connection.Config.t]
      in
      Ocaml_reddit.Connection.create config
    in
    List.range 0 700
    |> Deferred.List.iter ~how:(`Max_concurrent_jobs 25) ~f:(fun i ->
           Log.Global.info_s [%message "about to send request" (i : int)];
           test connection i)
;;

let () = Command.run (Command.async command ~summary:"test the thing")
