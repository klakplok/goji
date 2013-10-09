(****************************************************************************)
(* GOJI (JavaScript Interface Generator for OCaml)                          *)
(* This file is published under the CeCILL licence                          *)
(* (C) 2013 Benjamin Canou                                                  *)
(****************************************************************************)

(** A main entry point for conciseness. *)

module AST = Goji_ast
module DSL = Goji_dsl
module Registry = Goji_registry
module Grab = Goji_grab
module License = Goji_license
  
include AST
include DSL
include Registry
