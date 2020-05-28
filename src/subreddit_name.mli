(** [Subreddit_name] disregards "r/" and "/r/" prefixes and uses caseless hashes and comparisons. *)

open! Core
include Identifiable.S

val user_subreddit : Username.t -> t
