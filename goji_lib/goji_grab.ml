(****************************************************************************)
(* GOJI (JavaScript Interface Generator for OCaml)                          *)
(* This file is published under the CeCILL licence                          *)
(* (C) 2013 Benjamin Canou                                                  *)
(****************************************************************************)

open Printf

type result = Ok | Failures of string list
type filename = string
type command = unit -> result

let run ?(wd = ".") (command : command) =
  let prev_cwd = Sys.getcwd () in
  Sys.chdir wd ;
  let res = command () in
  Sys.chdir prev_cwd ;
  res
  
let shell fmt =
  kprintf (fun cmd () ->
      let res = Sys.command cmd in
      if res = 0 then
        Ok
      else
        Failures [ sprintf "command '%s' exited with error code %d" cmd res ])
    fmt

let exec cmd : command = shell "%s" cmd

let write str fn : command = fun () ->
  try
    let fp = open_out fn in
    output_string fp str ;
    close_out fp ;
    Ok
  with e -> Failures [ Printexc.to_string e ]

let append str fn : command = fun () ->
  try
    let fp = open_out_gen [ Open_append ; Open_creat ] 0o644 fn in
    output_string fp str ;
    close_out fp ;
    Ok
  with e -> Failures [ Printexc.to_string e ]

let callback f : command = fun () ->
  try f () ; Ok
  with e -> Failures [ Printexc.to_string e ]

let if_exists fn ct cf : command = fun () ->
  if Sys.file_exists fn then ct () else cf ()

let rec sequence (shells : command list) : command = fun () ->
  match shells with
  | [] -> Ok
  | shell :: shells ->
    match shell () with
    | Ok -> sequence shells ()
    | err -> err

let rec options (shells : command list) : command = fun () ->
  let rec loop acc shells =
    match shells with
    | [] -> Failures acc
    | shell :: shells ->
      match shell () with
      | Ok -> Ok
      | Failures errs -> loop (errs @ acc) shells
  in loop [] shells

let nop : command = fun () -> Ok

let http_get url dest : command =
  options [
    shell "wget %s -O %s" url dest ;
    shell "curl %s > %s" url dest
  ]

let git_clone url : command =
  shell "git clone %s" url

let darcs_get url : command =
  shell "darcs get %s" url

let unzip f : command =
  shell "unzip %s" f

let untar f : command =
  shell "tar xf %s" f

let remove f : command =
  callback (fun () -> Sys.remove f)

let rename f_from f_to : command =
  callback (fun () -> Sys.rename f_from f_to)
