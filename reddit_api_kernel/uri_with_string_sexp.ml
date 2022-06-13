open! Core
include Uri_sexp

let sexp_of_t t : Sexp.t = Atom (Uri.to_string t)
let t_of_sexp sexp = Uri.of_string ([%of_sexp: string] sexp)
