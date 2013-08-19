(****************************************************************************)
(* GOJI (JavaScript Interface Generator for OCaml)                          *)
(* This file is published under the CeCILL licence                          *)
(* (C) 2013 Benjamin Canou                                                  *)
(****************************************************************************)

open Printf

let error fmt =
  let write str =
    Printf.fprintf stderr "\027[31mError: \027[0m%s\n%!" str ;
    exit 1
  in
  Format.kprintf write fmt

let warning ?(level = 0) fmt =
  let write str =
    if !Goji_config.warning_level >= level then
      Printf.fprintf stderr "\027[33mWarning: \027[0m%s\n%!" str
  in
  Format.kprintf write fmt
    
let debug ?(level = 0) fmt =
  let write str =
    if !Goji_config.debug_level >= level then
      Printf.fprintf stderr "\027[32mDebug: \027[0m%s\n%!" str
  in
  Format.kprintf write fmt

let fdebug ?(level = 0) fp fmt =
  let write str =
    if !Goji_config.debug_level >= level then
      Printf.fprintf fp "%s%!" str
  in
  Format.kprintf write fmt

let ensure b fmt =
  let write str =
  if not b then
    error "%s" str
  in
  Printf.kprintf write fmt

let ensure_all f l fmt =
  let write str =
  if not (List.for_all f l) then
    error "%s" str
  in
  Printf.kprintf write fmt
