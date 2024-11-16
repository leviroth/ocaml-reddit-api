open! Core

module Password = struct
  type t =
    { client_id : string
    ; client_secret : string
    ; password : string
    ; username : string
    }
  [@@deriving sexp]
end

module Refresh_token = struct
  type t =
    { client_id : string
    ; client_secret : string option
    ; refresh_token : string
    }
  [@@deriving sexp]
end

module Userless_confidential = struct
  type t =
    { client_id : string
    ; client_secret : string
    }
  [@@deriving sexp]
end

module Userless_public = struct
  type t =
    { client_id : string
    ; device_id : string option
    }
  [@@deriving sexp]

  let device_id_or_default t =
    Option.value t.device_id ~default:"DO_NOT_TRACK_THIS_DEVICE"
  ;;
end

type t =
  | Password of Password.t
  | Refresh_token of Refresh_token.t
  | Userless_confidential of Userless_confidential.t
  | Userless_public of Userless_public.t
[@@deriving sexp]

let client_id t =
  match t with
  | Password { client_id; _ }
  | Refresh_token { client_id; _ }
  | Userless_confidential { client_id; _ }
  | Userless_public { client_id; _ } -> client_id
;;

let client_secret t =
  match t with
  | Refresh_token { client_secret = None; _ } | Userless_public _ -> None
  | Password { client_secret; _ }
  | Refresh_token { client_secret = Some client_secret; _ }
  | Userless_confidential { client_secret; _ } -> Some client_secret
;;

let basic_auth_string t =
  let client_id = client_id t in
  let client_secret = Option.value (client_secret t) ~default:"" in
  Cohttp.Auth.string_of_credential (`Basic (client_id, client_secret))
;;

let auth_header t = Cohttp.Header.init_with "Authorization" (basic_auth_string t)

let access_token_request_params t =
  match t with
  | Password { username; password; _ } ->
    [ "grant_type", [ "password" ]; "username", [ username ]; "password", [ password ] ]
  | Refresh_token { refresh_token; _ } ->
    [ "grant_type", [ "refresh_token" ]; "refresh_token", [ refresh_token ] ]
  | Userless_confidential _ -> [ "grant_type", [ "client_credentials" ] ]
  | Userless_public public_credentials ->
    [ "grant_type", [ "https://oauth.reddit.com/grants/installed_client" ]
    ; "device_id", [ Userless_public.device_id_or_default public_credentials ]
    ]
;;
