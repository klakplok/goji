(****************************************************************************)
(* GOJI (JavaScript Interface Generator for OCaml)                          *)
(* This file is published under the CeCILL licence                          *)
(* (C) 2013 Benjamin Canou                                                  *)
(****************************************************************************)

open Goji_ast

let parse_error msg =
  failwith ("parse_error: " ^ msg)

let parse_type_variable str =
  if str = "" then
    parse_error "empty type variable name"
  else
    match str.[0] with
    | '-' when String.length str > 1 && str.[1] = '\''->
      Some Contravariant, String.sub str 2 (String.length str - 2)
    | '-' -> Some Contravariant, String.sub str 1 (String.length str - 1)
    | '+' when String.length str > 1 && str.[1] = '\''->
      Some Covariant, String.sub str 2 (String.length str - 2)
    | '+' -> Some Covariant, String.sub str 1 (String.length str - 1)
    | '\'' -> None, String.sub str 1 (String.length str - 1)
    | _ -> None, str

let rec parse_qualified_name str =
  let len = String.length str in
  let rec do_cap acc lst cur =
    if cur = len then
      parse_error "unterminated name"
    else
      match str.[cur] with
      | 'a'..'z' | 'A'..'Z' | '0'..'9' | '_' | '\'' -> do_cap acc lst (succ cur)
      | '.' ->
	let acc = String.sub str lst (cur - lst) :: acc in
	do_next acc (succ cur)
      | c -> parse_error "unexpected character in name"
  and do_min acc lst cur =
    if cur = len then
      if lst = cur then
	parse_error "unterminated name"
      else
	(List.rev acc, String.sub str lst (cur - lst))
    else
    match str.[cur] with
    | 'a'..'z' | 'A'..'Z' | '0'..'9' | '_' | '\'' -> do_min acc lst (succ cur)
    | '.' -> parse_error "bad name (modules must start with capitals)"
    | c -> parse_error "unexpected character in name"
  and do_next acc cur =
    if cur = len then
      parse_error "unterminated name"
    else
      match str.[cur] with
      | 'a'..'z' | '_' -> do_min acc cur (succ cur)
      | 'A'..'Z' -> do_cap acc cur (succ cur)
      | c -> parse_error "unexpected character in name"
  in
  do_next [] 0

let parse_js_global js =
  match Str.(split (regexp "\\.") js) with
  | [] -> invalid_arg "Goji_syntax.parse_js_global"
  | glo :: path ->
    List.fold_left
      (fun r f -> Field (r, f))
      (Global glo)
      path

