(** [Username] disregards "u/" and "/u/" prefixes and uses caseless hashes and comparisons. *)

open! Core
include Identifiable.S
