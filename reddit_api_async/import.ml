open! Core
open! Async

let log = Log.create ~level:`Error ~output:[] ~on_error:`Raise ()
