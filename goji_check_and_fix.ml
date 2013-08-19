(****************************************************************************)
(* GOJI (JavaScript Interface Generator for OCaml)                          *)
(* This file is published under the CeCILL licence                          *)
(* (C) 2013 Benjamin Canou                                                  *)
(****************************************************************************)

(** Binding description checking (and quick & dirty fixing when possible) *)

(** Checks that the names of packages / variables / modules names
    respect OCaml restrictions and that they don't collide *)
let check_names ?(fix = true) () =
  let open Goji_messages in
  let message fmt =
    if !Goji_config.fix_case then
      warning ~level:2 fmt
    else
      error fmt
  in
  let rec check_module_name context name =
    match name.[0] with
    | 'a'..'z' -> 
      message "invalid case for %s %S" context name ;
      name.[0] <- Char.uppercase name.[0] ;
      check_tail context name
    | 'A'..'Z' -> check_tail context name
    | _ -> error "invalid %s %S" context name
  and check_variable_name context name =
    match name.[0] with
    | 'A'..'Z' -> 
      message "invalid case for %s %S" context name ;
      name.[0] <- Char.lowercase name.[0] ;
      check_tail context name
    | 'a'..'z' -> check_tail context name
    | _ -> error "invalid %s %S" context name
  and check_tail context name = 
    for i = 1 to String.length name - 1 do
      match name.[i] with
      | 'a' .. 'z' | 'A' .. 'Z' | '_' | '0'..'9' -> ()
      | c -> error "invalid character %C in %s %S" c context name
    done
  in
  let open Goji_registry in
  let rec check_rec package =
    check_variable_name "package" package.package_name ;
    iter_components
      (fun component ->
	check_module_name "component" component.name)
      package ;
    iter_subpackages check_rec package
  in
  iter_packages check_rec
