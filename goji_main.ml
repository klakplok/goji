(****************************************************************************)
(* GOJI (JavaScript Interface Generator for OCaml)                          *)
(* This file is published under the CeCILL licence                          *)
(* (C) 2013 Benjamin Canou                                                  *)
(****************************************************************************)

(** Main entry moint of the tool. *)

open Cmdliner

(* Options common to all commands *)

let copts_sect = "COMMON OPTIONS"

let help_secs = [ 
  `S copts_sect ; 
  `P "Use `$(mname) $(i,COMMAND) --help' for help on a specific command." ;
  `P "These options are common to all commands." ;
  `S "SEE ALSO" ;
  `P "You can read the README.md file available in the \
      GitHub repository for a quick overview. \
      When writing bindings, have a look at the generated \
      ocamldoc of the goji_lib package for details." ; ]

let copts_t = 
  let docs = copts_sect in 
  let debug_t =
    let doc = "Set debug messages display policy, \
               either none, all or the level between 0 and 3 " in
    Arg.(value
	 & opt
	   (enum ["none", -1 ; "0", 0 ; "1", 1 ; "2", 2 ; "3", 3 ; "all", max_int]) (-1)
	 & info ~docs ~doc  ["d" ; "debug"])
  in 
  let warn_t =
    let doc = "Set warning display policy, \
               either none, all or the level between 0 and 3." in
    Arg.(value
	 & opt
	   (enum ["none", -1 ; "0", 0 ; "1", 1 ; "2", 2 ; "3", 3 ; "all", max_int]) 0
	 & info ~docs ~doc  ["w" ; "warn"])
  in 
  let pack_copts warning_level debug_level =
    (warning_level, debug_level)
  in
  Term.(pure pack_copts $ warn_t $ debug_t)

let with_copts cont (warning_level, debug_level) =
  Goji_config.warning_level := warning_level ;
  Goji_config.debug_level := debug_level ;
  cont

(* Commands *)

let generate_cmd = 
  let open Arg in
  let dir_t = 
    let doc = "The path to the output base directory (which should exist)." in
    let info = info [ "d" ; "base-dir"] ~docv:"DIR" ~doc in
    value & opt dir Filename.current_dir_name & info
  in
  let fix_t = 
    let doc = "If this flag is passed, the case of modules and packages will be \
               fixed when possible, producing level 1 warnings instead of errors." in
    let yes = true, info [ "fix-case"] ~doc in
    let doc = "If this flag is passed, any erroneous case in a module or package name \
               will result in an error." in
    let no = false, info [ "dont-fix-case"] ~doc in
    last & vflag_all [ true ] [ no ; yes ]
  in
  let modules_t =
    let doc = "The list of .ml or .cmxs files that define bindings." in
    value & pos_all file [] & info [] ~docv:"MODULE" ~doc
  in
  let doc = "Generate bindings" in
  let man = [
    `S "DESCRIPTION";
    `P "Generate bindings from descriptions. \
        Bindings are described in OCaml sources that link against the \
        goji_lib OCamlFind package. Writing a binding means writing its defintion \
        as an AST using the Goji_ast module or with the help of the Goji_dsl \
        module, and then registering them for generation using the Goji_registry module. \
        The OCaml sources (precompiled or not) are dynlinked sequentially and then \
        bindings are generated for all registered packages and components." ] @ help_secs
  in
  Term.(pure (with_copts Goji_generate.main) $ copts_t $ dir_t $ fix_t $ modules_t),
  Term.info "generate" ~sdocs:copts_sect ~doc ~man

let jslink_cmd = 
  let open Arg in
  let dir_t = 
    let doc = "The path to the output base directory (which should exist)." in
    let info = info [ "d" ; "base-dir"] ~docv:"DIR" ~doc in
    value & opt dir Filename.current_dir_name & info
  in
  let out_t = 
    let doc = "The name of the output JavaScript file." in
    let info = info [ "o" ; "output-file"] ~docv:"OUT" ~doc in
    value & opt string "jslibs.js" & info
  in
  let packages_t =
    let doc = "The list of ocamlfind packages." in
    value & pos_all string [] & info [] ~docv:"PACKAGE" ~doc
  in
  let doc = "Generate an adequate JavaScript loader for a set of goji bindings." in
  let man = [
    `S "DESCRIPTION";
    `P "Generate a JavaScript file that contains the code necessary for a set \
        of bindings to work. It may also output other auxiliary files along \
        with the main JavaScript depending on the binding description. \
        To list these auxiliary files, use the --list option. \
        You can also use the --remove option to remove them." ] @ help_secs
  in
  Term.(pure (with_copts Goji_jslink.main) $ copts_t $ dir_t $ out_t $ packages_t),
  Term.info "jslink" ~sdocs:copts_sect ~doc ~man

let default_cmd = 
  let doc = "OCaml bindings generator for JavaScript libraries" in 
  let man = help_secs in
  Term.(ret (pure (fun _ -> `Help (`Pager, None)) $ copts_t)),
  Term.info "goji" ~version:Goji_config.version ~sdocs:copts_sect ~doc ~man

(* Main interface *)

let cmds = [
  generate_cmd ;
  jslink_cmd
]

let () =
  match Term.eval_choice default_cmd cmds with 
  | `Error _ -> exit 1
  | _ -> exit 0
