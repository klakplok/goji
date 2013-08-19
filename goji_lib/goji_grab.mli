(****************************************************************************)
(* GOJI (JavaScript Interface Generator for OCaml)                          *)
(* This file is published under the CeCILL licence                          *)
(* (C) 2013 Benjamin Canou                                                  *)
(****************************************************************************)

(** Scripting library to grab and format JavaScript sources *)

(** The type of delayed commands, to be executed explicitly by the
    {!run} function. *)
type command

(** An alias for file names. *)
type filename = string

(** The result of running a command. Either successful or
    unsuccessful, in which case every failure that happened during the
    run is returned for display / analysis. *)
type result = Ok | Failures of string list

(** Runs a command and returns its result. The [wd] parameter is an
    optional directory in which to run the command. In any case, the
    current directory is restored afterwards. *)
val run : ?wd:string -> command -> result

(** A command that execs a shell command. Returns [Ok] if the result
    of the shell command is 0. *)
val exec : string -> command

(** Writes the contents of a string to a file. *)
val write : string -> filename -> command

(** Writes the contents of a string at the end of a file. *)
val append : string -> filename -> command

(** Lift an OCaml function as a command *)
val callback : (unit -> unit) -> command

(** Runs the first command if the file exists the second otherwise. *)
val if_exists : filename -> command -> command -> command

(** Lifts several command as one, execution is stopped on the first error. *)
val sequence : command list -> command

(** Behaves as the first command to succeed. Stores all the failures. *)
val options : command list -> command

(** Void command. *)
val nop : command

(** Downloads an URL to a file. *)
val http_get : string -> filename -> command

(** Performs a [git clone]. *)
val git_clone : string -> command

(** Performs a [darcs get]. *)
val darcs_get : string -> command

(** Unzips a file. *)
val unzip : filename -> command

(** Untars a file. *)
val untar : filename -> command

(** Removes a file. *)
val remove : filename -> command

(** Renames / moves a file. *)
val rename : filename -> filename -> command
