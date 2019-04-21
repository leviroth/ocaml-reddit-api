open! Core
open! Async

let test auth =
  let%bind () = Ocaml_reddit.Connection.Auth.get_token auth in
  let uri = Uri.of_string "https://oauth.reddit.com/api/v1/me" in
  let%bind response, body = Ocaml_reddit.Connection.Auth.call auth `GET uri in
  print_s [%message "" (response : Cohttp.Response.t) (body : Cohttp_async.Body.t)];
  return ()
;;

let command =
  let open Command.Let_syntax in
  let%map_open auth_config_path = anon ("CONFIG" %: string) in
  fun () ->
    let auth =
      let config =
        Sexp.load_sexp_conv_exn
          auth_config_path
          [%of_sexp: Ocaml_reddit.Connection.Auth.Config.t]
      in
      Ocaml_reddit.Connection.Auth.create config ()
    in
    test auth
;;

let () = Command.run (Command.async command ~summary:"test the thing")
