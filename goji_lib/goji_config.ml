(****************************************************************************)
(* GOJI (JavaScript Interface Generator for OCaml)                          *)
(* This file is published under the CeCILL licence                          *)
(* (C) 2013 Benjamin Canou                                                  *)
(****************************************************************************)

(** Used by {!Goji_messages.debug} to filter debug messages. The more
    important the debug message, the lower its level. Any debug
    message equal to or above this level will be displayed. The
    [debug_level] is [-1] if unset explicitly so no debug message is
    output by default. *)
let debug_level = ref (-1)

(** Used by {!Goji_messages.warning} to filter warning. The more
    important the warning, the lower its level. Any warning equal to
    or above this level will be displayed. The [warning_level] is [0]
    if unset explicitly so only the most important warnings are
    displayed. *)
let warning_level = ref 0

(** The version string *)
let version = "0.1"
