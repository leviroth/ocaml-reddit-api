(** [Username] is a string identifier module that does some normalization:

   - Hashes and comparisons are caseless.
   - "u/" and "/u/" prefixes are dropped.
*)

open! Core
include Identifiable.S
