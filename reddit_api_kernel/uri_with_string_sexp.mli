(** [Uri_with_string_sexp] is {!module:Uri_sexp} with a different sexp
    serialization:

    [sexp_of_t t = Atom (Uri.to_string t)]
*)
open! Core

include module type of struct
  include Uri_sexp
end
