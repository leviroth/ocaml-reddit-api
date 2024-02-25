(** [Password] credentials correspond to Reddit's
    {{:https://github.com/reddit-archive/reddit/wiki/oauth2-app-types#script}"script"}
    app type.

    @see < https://datatracker.ietf.org/doc/html/rfc6749#section-4.3.2 > The RFC
    6749 section describing the corresponding access token request. *)
module Password : sig
  type t =
    { client_id : string
    ; client_secret : string
    ; password : string
    ; username : string
    }
  [@@deriving sexp]
end

(** [Refresh_token] credentials correspond to Reddit's
    {{:https://github.com/reddit-archive/reddit/wiki/oauth2-app-types#web-app}"web
    app"} and
    {{:https://github.com/reddit-archive/reddit/wiki/oauth2-app-types#installed-app}"installed-app"}
    app types.

    @see < https://praw.readthedocs.io/en/stable/tutorials/refresh_token.html
    > {{:https://praw.readthedocs.io/}PRAW}'s documentation on refresh tokens
    for advice on obtaining a refresh token, which is currently outside the
    scope of this project.

    @see < https://datatracker.ietf.org/doc/html/rfc6749#section-6 > The RFC
    6749 section describing the corresponding access token request. *)
module Refresh_token : sig
  type t =
    { client_id : string
    ; client_secret : string option
          (** This field is present for web apps and absent for installed apps. *)
    ; refresh_token : string
    }
  [@@deriving sexp]
end

module Userless_confidential : sig
  type t =
    { client_id : string
    ; client_secret : string
    }
  [@@deriving sexp]
end

module Userless_public : sig
  type t =
    { client_id : string
    ; device_id : string option
    }
  [@@deriving sexp]

  val device_id_or_default : t -> string
end

type t =
  | Password of Password.t
  | Refresh_token of Refresh_token.t
  | Userless_confidential of Userless_confidential.t
  | Userless_public of Userless_public.t
[@@deriving sexp]

val basic_auth_string : t -> string
val auth_header : t -> Cohttp.Header.t
val access_token_request_params : t -> (string * string list) list
