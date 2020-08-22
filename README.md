`Reddit_api` is an OCaml client library for Reddit's API.

# Installation

We provide a custom opam repository for a dependency not in the main opam
repository:

```bash
$ opam repo add leviroth https://github.com/leviroth/opam-repository.git
$ opam install reddit_api
```

# Documentation

[Here](https://leviroth.github.io/ocaml-reddit-api/reddit_api/index.html). I
recommend the [`Api`
module](https://leviroth.github.io/ocaml-reddit-api/reddit_api/Reddit_api/Api/index.html)
as an entry point.

# Design philosophy and caveats

`Reddit_api` aims to help users navigate the Reddit API via OCaml types. For
example, many HTTP endpoints have parameters that cannot be used in conjunction
with each other. In such cases, we wrap these parameters in variant types so
that users don't have to discover the conflict for themselves. For another
example, we try to wrap outputs in types that express the full range of
possible responses from Reddit. If `Reddit_api` ever raises, we regard this as
a bug and prefer to express this possibility via a type rather than forcing
users to anticipate exceptions. Conversely, if some Response type `t` always
has a value of type `v`, we try to provide a function `t -> v` rather than
providing `t -> v option` and forcing users to guess when the result might be
`None`.

The caveat is that this is hard. Reddit's API is not very well documented.
Determining which inputs and outputs are legal is largely a matter of trial and
error. At any given time, it's likely that we allow some invalid combination of
inputs, or forbid a valid combination, or fail to handle some valid response.

If this caveat bites you, reports and pull requests are certainly welcome. To
facilitate workarounds, we also provide:

1. A `?param_list_override:((string * string list) list -> (string * string
   list) list)` option on each API endpoint that allows the HTTP parameters to
   be manipulated directly.

2. An `Api.Raw` module that provides all the same functions but does not
   parsing of the responses.

# Credits

Thanks to [PRAW](https://github.com/praw-dev/praw/) for providing innumerable
examples of Reddit API client code.
