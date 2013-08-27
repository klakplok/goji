(****************************************************************************)
(* GOJI (JavaScript Interface Generator for OCaml)                          *)
(* This file is published under the CeCILL licence                          *)
(* (C) 2013 Benjamin Canou                                                  *)
(****************************************************************************)

(** Binding description checking (and quick & dirty fixing when possible) *)

open Goji_messages
open Goji_ast

(** Checks that the last parameter of functions is not an optional,
    and adds a unit parameter to functions with zero argument. Also
    checks that parameter names do not collide. *)
let check_params ?(reorder_params = true) () =  
  let fake_unit_param = Curry, "()", Nodoc, Value (Void, Var "<fake>") in
  let module SS = Set.Make (String) in
  let ensure_last_is_positional l =
    let rec add_rec acc = function
      | (Curry | Labeled), _, _, _ as p1 :: tl ->
	if acc = [] && not reorder_params then
	  warning "last argument is optional, you should correct this or use --reorder-params" ;
	List.rev tl @ acc @ [ p1 ]
      | Optional, _, _, _ as p :: tl ->
	add_rec (p :: acc) tl
      | [] -> List.rev (fake_unit_param :: acc)
    in add_rec [] (List.rev l)
  in
  let check_unique_names l =
    let rec check_rec s l =
      match l with
      | (_, n, _, _) :: _ when SS.mem n s ->
	error "duplicate parameter %S" n
      | (_, n, _, _) :: ps ->
	check_rec (SS.add n s) ps
      | [] -> ()
    in check_rec SS.empty l
  in
  let fix_params = object
    inherit map as mom
    method binding = function
    | Method (abbrv, s, pl, body, ret, doc) ->
      let pl = if pl = [] then [] else ensure_last_is_positional pl in
      check_unique_names pl ;
      mom # binding (Method (abbrv, s, pl, body, ret, doc))
    | Function (s, pl, body, ret, doc) ->
      let pl = ensure_last_is_positional pl in
      check_unique_names pl ;
      mom # binding (Function (s, pl, body, ret, doc))
    | e -> mom # binding e
    method mapping = function
    | Callback (pl, v) ->
      let pl = ensure_last_is_positional pl in
      check_unique_names pl ;
      mom # mapping (Callback (pl, v))
    | Handler (pl, v, cb) ->
      let pl = ensure_last_is_positional pl in
      check_unique_names pl ;
      mom # mapping (Handler (pl, v, cb))
    | e -> mom # mapping e
  end in
  let open Goji_registry in
  iter_packages
    (iter_components
       (fun component ->
	 component.elements <-
	   (List.map
	      (fix_params # binding)
	      component.elements)))


(** Checks that the names of packages / variables / modules names
    respect OCaml restrictions and that they don't collide *)
let check_names ?(fix_case = true) () =
  let message fmt =
    if fix_case then
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
  iter_packages
    (fun package ->
      check_variable_name "package" package.package_name ;
      iter_components
	(fun component ->
	  check_module_name "component" component.name)
	package)
