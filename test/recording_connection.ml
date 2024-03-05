open! Core
open Async
open Reddit_api_async

module Placeholders : sig
  type t

  val create : unit -> t
  val add : t -> secret:string -> placeholder:string -> unit
  val filter_string : t -> string -> string
  val insert_dummy_strings : t -> string -> string
end = struct
  type t = string Hashtbl.M(String).t

  let create () = Hashtbl.create (module String)

  let add t ~secret ~placeholder =
    let placeholder = sprintf "<%s>" (String.uppercase placeholder) in
    Hashtbl.add_exn t ~key:placeholder ~data:secret
  ;;

  let filter_string t string =
    Hashtbl.fold t ~init:string ~f:(fun ~key:placeholder ~data:secret string ->
        String.substr_replace_all string ~pattern:secret ~with_:placeholder)
  ;;

  let insert_dummy_strings t string =
    Hashtbl.fold t ~init:string ~f:(fun ~key:placeholder ~data:secret string ->
        String.substr_replace_all string ~pattern:placeholder ~with_:secret)
  ;;
end

module Cassette = struct
  module type S = sig
    include Connection.For_testing.Cohttp_client_wrapper

    val seal : unit -> unit
    val time_source : Time_source.t
  end

  module Interaction = struct
    let map_headers headers ~f = Cohttp.Header.map (fun _key value -> f value) headers

    module Request = struct
      module T = struct
        type t =
          | Get of
              { uri : Uri_with_string_sexp.t
              ; headers : Cohttp.Header.t
              }
          | Post_form of
              { uri : Uri_with_string_sexp.t
              ; headers : Cohttp.Header.t
              ; params : (string * string list) list
              }
        [@@deriving sexp, compare]

        let uri (Get { uri; _ } | Post_form { uri; _ }) = uri
      end

      include T
      include Comparable.Make (T)

      let map t ~f =
        let map_uri uri = Uri.to_string uri |> f |> Uri.of_string in
        let map_params params =
          List.map params ~f:(fun (key, values) -> key, List.map values ~f)
        in
        match t with
        | Get { uri; headers } ->
          Get { uri = map_uri uri; headers = map_headers headers ~f }
        | Post_form { uri; headers; params } ->
          Post_form
            { uri = map_uri uri
            ; headers = map_headers headers ~f
            ; params = map_params params
            }
      ;;
    end

    type t =
      { request : Request.t
      ; response : Cohttp.Response.t * string
      }
    [@@deriving sexp]

    let map { request; response = response, body } ~f =
      let request = Request.map request ~f in
      let response =
        { response with headers = map_headers response.headers ~f }, f body
      in
      { request; response }
    ;;
  end

  let recording filename placeholders : (module S) =
    (module struct
      module Cohttp_client_wrapper =
        (val Connection.For_testing.live_cohttp_client ~user_agent:"ocaml-reddit testing")

      let queue : Interaction.t Queue.t = Queue.create ()

      let save_interaction request response =
        let%bind response_to_write =
          let response, body = response in
          let%bind body = Cohttp_async.Body.to_string body in
          return (response, body)
        in
        Queue.enqueue queue { request; response = response_to_write };
        return (Tuple2.map_snd response_to_write ~f:Cohttp_async.Body.of_string)
      ;;

      let get uri ~headers =
        let%bind response = Cohttp_client_wrapper.get uri ~headers >>| Result.ok_exn in
        let%bind response = save_interaction (Get { uri; headers }) response in
        return (Ok response)
      ;;

      let post_form uri ~headers ~params =
        let%bind response =
          Cohttp_client_wrapper.post_form uri ~headers ~params >>| Result.ok_exn
        in
        let%bind response =
          save_interaction (Post_form { uri; headers; params }) response
        in
        return (Ok response)
      ;;

      let is_access_token_interaction (interaction : Interaction.t) =
        String.is_substring
          (Interaction.Request.uri interaction.request |> Uri.to_string)
          ~substring:"api/v1/access_token"
      ;;

      let seal () =
        printf "Please move the following to test/%s\n\n" filename;
        Queue.iter queue ~f:(fun interaction ->
            (match is_access_token_interaction interaction with
            | false -> ()
            | true ->
              let _, body = interaction.response in
              let json = Jsonaf.of_string body in
              (match Jsonaf.member "access_token" json with
              | None -> ()
              | Some json ->
                let token = Jsonaf.string_exn json in
                Placeholders.add placeholders ~secret:token ~placeholder:"access_token"));
            Interaction.map interaction ~f:(Placeholders.filter_string placeholders)
            |> Interaction.sexp_of_t
            |> Sexp.output_mach Out_channel.stdout);
        printf "\n\nPlease move the above to test/%s" filename
      ;;

      let time_source = Time_source.wall_clock ()
    end)
  ;;

  let reading filename placeholders : (module S) =
    (module struct
      let queue : Interaction.t Queue.t =
        In_channel.with_file filename ~f:(fun in_channel ->
            Sexp.input_sexps in_channel
            |> List.map ~f:Interaction.t_of_sexp
            |> Queue.of_list)
      ;;

      let dequeue_response () =
        let ({ request; response } : Interaction.t) =
          Queue.dequeue_exn queue
          |> Interaction.map ~f:(Placeholders.insert_dummy_strings placeholders)
        in
        let response = Tuple2.map_snd response ~f:Cohttp_async.Body.of_string in
        request, response
      ;;

      let headers_equal =
        Comparable.lift
          [%compare: (String.Caseless.t * string) list]
          ~f:Cohttp.Header.to_list
        |> Base.Comparable.equal
      ;;

      let get uri ~headers =
        let request, response = dequeue_response () in
        let fail () =
          raise_s
            [%message
              "Test request did not match record"
                (uri : Uri_with_string_sexp.t)
                (headers : Cohttp.Header.t)
                ~recorded_request:(request : Interaction.Request.t)]
        in
        match request with
        | Post_form _ -> fail ()
        | Get request ->
          (match Uri.equal uri request.uri && headers_equal headers request.headers with
          | false -> fail ()
          | true -> return (Ok response))
      ;;

      let post_form uri ~headers ~params =
        let request, response = dequeue_response () in
        let fail () =
          raise_s
            [%message
              "Test request did not match record"
                (uri : Uri_with_string_sexp.t)
                (headers : Cohttp.Header.t)
                (params : (string * string list) list)
                ~recorded_request:(request : Interaction.Request.t)]
        in
        match request with
        | Get _ -> fail ()
        | Post_form request ->
          (match
             Uri.equal uri request.uri
             && headers_equal headers request.headers
             && [%equal: (string * string list) list] params request.params
           with
          | false -> fail ()
          | true -> return (Ok response))
      ;;

      let seal () = assert (Queue.is_empty queue)

      let time_source =
        Time_source.read_only (Time_source.create ~now:Time_ns.max_value_representable ())
      ;;
    end)
  ;;

  let with_t filename ~credentials ~f =
    let placeholders = Placeholders.create () in
    (match (credentials : Credentials.t) with
    | Password { client_id; client_secret; username; password } ->
      Placeholders.add placeholders ~secret:client_id ~placeholder:"client_id";
      Placeholders.add placeholders ~secret:client_secret ~placeholder:"client_secret";
      Placeholders.add placeholders ~secret:password ~placeholder:"password";
      Placeholders.add placeholders ~secret:username ~placeholder:"username"
    | Refresh_token { client_id; client_secret; refresh_token } ->
      Placeholders.add placeholders ~secret:client_id ~placeholder:"client_id";
      Option.iter client_secret ~f:(fun secret ->
          Placeholders.add placeholders ~secret ~placeholder:"client_secret");
      Placeholders.add placeholders ~secret:refresh_token ~placeholder:"refresh_token"
    | Userless_confidential { client_id; client_secret } ->
      Placeholders.add placeholders ~secret:client_id ~placeholder:"client_id";
      Placeholders.add placeholders ~secret:client_secret ~placeholder:"client_secret"
    | Userless_public ({ client_id; device_id = _ } as public_credentials) ->
      Placeholders.add placeholders ~secret:client_id ~placeholder:"client_id";
      Placeholders.add
        placeholders
        ~secret:(Credentials.Userless_public.device_id_or_default public_credentials)
        ~placeholder:"device_id");
    Placeholders.add
      placeholders
      ~secret:(Credentials.basic_auth_string credentials)
      ~placeholder:"authorization";
    let%bind file_exists = Sys.file_exists_exn filename in
    let (module Cassette) =
      match file_exists with
      | true -> reading filename placeholders
      | false -> recording filename placeholders
    in
    let connection =
      Connection.For_testing.create
        (module Cassette)
        credentials
        ~time_source:Cassette.time_source
    in
    Monitor.protect
      (fun () -> f connection)
      ~finally:(fun () ->
        Cassette.seal ();
        return ())
  ;;
end

let with_cassette filename ~credentials ~f = Cassette.with_t filename ~credentials ~f
