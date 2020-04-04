open! Core

type t =
  [ `Null
  | `False
  | `True
  | `String of string
  | `Number of string
  | `Object of (string * t) list
  | `Array of t list
  ]
[@@deriving sexp]

include (Jsonaf : module type of Jsonaf with type t := t)

let of_string string = of_string string |> Result.map_error ~f:Error.of_string
let of_string_exn string = of_string string |> Or_error.ok_exn

let to_map_exn t =
  match t with
  | `Object l -> String.Map.of_alist_exn l
  | _ -> raise_s [%message "Json is not an object" (t : t)]
;;

let to_map t = Or_error.try_with (fun () -> to_map_exn t)

let get_string_exn t =
  match t with
  | `String s -> s
  | _ -> raise_s [%message "Json is not a string" (t : t)]
;;

let get_string t = Or_error.try_with (fun () -> get_string_exn t)

let get_int_exn t =
  match t with
  | `Number s -> Int.of_string s
  | _ -> raise_s [%message "Json is not a number" (t : t)]
;;

let get_int t = Or_error.try_with (fun () -> get_int_exn t)

let find_exn t ~key =
  match t with
  | `Object l -> List.Assoc.find_exn l key ~equal:String.equal
  | _ -> raise_s [%message "Json is not an object" (t : t)]
;;

let get_array_exn t =
  match t with
  | `Array array -> array
  | _ -> raise_s [%message "Json is not an array" (t : t)]
;;

let get_array t = Or_error.try_with (fun () -> get_array_exn t)
let find t ~key = Or_error.try_with (fun () -> find_exn t ~key)

let index_exn t ~index =
  match t with
  | `Array l -> List.nth l index |> Option.value_exn
  | _ -> raise_s [%message "Json is not an array" (t : t)]
;;

let index t ~index = Or_error.try_with (fun () -> index_exn t ~index)

let get_bool_exn t =
  match t with
  | `True -> true
  | `False -> false
  | _ -> raise_s [%message "Json is not a bool" (t : t)]
;;

let get_bool t = Or_error.try_with (fun () -> get_bool_exn t)
