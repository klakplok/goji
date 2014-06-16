(****************************************************************************)
(* GOJI (JavaScript Interface Generator for OCaml)                          *)
(* This file is published under the CeCILL licence                          *)
(* (C) 2013 Benjamin Canou                                                  *)
(****************************************************************************)

type 'a global
type ('a, 'b) local

val enter_package : string -> 'a global
val enter_module : string -> 'a global -> 'a global
val close_module : 'a global -> 'a global
val enter_definition : string -> 'a global -> ('a, 'b) local
val close_definition : ('a, 'b) local -> 'a global
val get_module : string list * string -> 'a global -> 'a global
val get_definition : string list * string -> 'a global -> 'a
val define_goji_var : string -> 'a -> ('a, 'b) local -> ('a, 'b) local
val define_ocaml_var : string -> 'a -> ('a, 'b) local -> ('a, 'b) local
val goji_var_exists : string -> ('a, 'b) local -> bool
val ocaml_var_exists : string -> ('a, 'b) local -> bool
val get_goji_var : string -> ('a, 'b) local -> 'b
val get_ocaml_var : string -> ('a, 'b) local -> 'b
