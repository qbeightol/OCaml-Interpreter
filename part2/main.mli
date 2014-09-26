(** [Main] is the entry point for the user in the 3110Caml interpreter.
    This module is responsible for parsing the command
    line arguments provided by the user and starting up the [Repl]
    loop. *)

(** [main] is the entry point of the interpreter. It parses the command
    line arguments and then starts the [Repl] loop with the empty
    environment. *)
val main : unit -> unit
