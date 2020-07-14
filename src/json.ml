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

let of_string string = of_string string |> Result.ok_or_failwith

let to_map t =
  match t with
  | `Object l -> String.Map.of_alist_exn l
  | _ -> raise_s [%message "Json is not an object" (t : t)]
;;

let get_string t =
  match t with
  | `String s -> s
  | _ -> raise_s [%message "Json is not a string" (t : t)]
;;

let get_int t =
  match t with
  | `Number s -> Int.of_string s
  | _ -> raise_s [%message "Json is not a number" (t : t)]
;;

let get_float t =
  match t with
  | `Number s -> Float.of_string s
  | _ -> raise_s [%message "Json is not a number" (t : t)]
;;

let find t ~key =
  match t with
  | `Object l -> List.Assoc.find_exn l key ~equal:String.equal
  | _ -> raise_s [%message "Json is not an object" (t : t)]
;;

let get_array t =
  match t with
  | `Array array -> array
  | _ -> raise_s [%message "Json is not an array" (t : t)]
;;

let index t ~index =
  match t with
  | `Array l -> List.nth l index |> Option.value_exn
  | _ -> raise_s [%message "Json is not an array" (t : t)]
;;

let get_bool t =
  match t with
  | `True -> true
  | `False -> false
  | _ -> raise_s [%message "Json is not a bool" (t : t)]
;;

let none_if_null t =
  match (t : t) with
  | `Null -> None
  | t -> Some t
;;
