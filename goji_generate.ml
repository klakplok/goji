(****************************************************************************)
(* GOJI (JavaScript Interface Generator for OCaml)                          *)
(* This file is published under the CeCILL licence                          *)
(* (C) 2013 Benjamin Canou                                                  *)
(****************************************************************************)

(** Implementation of the [generate] command. *)

(** Builds the .ml and .mli files for a specific package. *)
let generate_sources base_dir package =
  let open Goji_registry in
  iter_components
    (fun component ->
       (* compute file name *)
       let base_name = String.copy component.name in
       base_name.[0] <- Char.lowercase base_name.[0] ;
       let full_base_name = base_dir ^ "/" ^ base_name in
       (* generate ml *)
       let fp_impl = open_out (full_base_name ^ ".ml") in
       Goji_emit_struct.emit_implementation fp_impl component ;
       close_out fp_impl ;
       (* generate mli *)
       let fp_intf = open_out (full_base_name ^ ".mli") in
       Goji_emit_struct.emit_interface fp_intf component ;
       close_out fp_intf)
    package

(** Builds the directory for some package and returns its path. *)
let make_package_dir output_base_dir package =
  let open Goji_registry in
  let base_dir = output_base_dir ^ "/" ^ package.package_name in
  if not (Sys.file_exists base_dir) then
    Unix.mkdir base_dir 0o755 ;
  base_dir

(** Unifies the dependencies of all the components of a package. *)
let compute_depends package =
  let open Goji_registry in
  let accu = ref ("goji_runtime" :: package.package_depends) in
  let rec uniq acc = function
    | [] -> acc
    | e1 :: (e2 :: _ as es) when e1 = e2 -> uniq acc es
    | e :: es -> uniq (e :: acc) es
  in
  iter_components (fun c -> accu := c.depends @ !accu) package ;
  uniq [] (List.sort String.compare !accu)

(** Build the Makefile. *)
let generate_makefiles base_dir package js_present =
  let open Goji_registry in
  let fp_makefile = open_out (base_dir ^ "/Makefile") in
  let out fmt = Printf.fprintf fp_makefile fmt in
  let base_names =
    List.map
      (fun name ->
	let name = String.copy name in
	name.[0] <- Char.lowercase name.[0] ;
	name)
      (components package)
  and depends = compute_depends package in
  let rec sep s _ = function
    | [] -> ()
    | [e] -> out "%s" e
    | e :: es -> out "%s%s%a" e s (sep s) es
  in
  out "DEPENDS=%a\n"
    (sep ",") depends ;
  out "MLIS=%a\n"
    (sep " ") (List.map (fun m -> m ^ ".mli") base_names) ;
  out "CMOS=%a\n"
    (sep " ") (List.map (fun m -> m ^ ".cmo") base_names) ;
  out "CMIS=%a\n\n"
    (sep " ") (List.map (fun m -> m ^ ".cmi") base_names) ;
  out ".PHONY: all clean doc install uninstall\n\n" ;
  out "all: %s.cma\n\n"
    package.package_name ;
  out "%s.cma: $(CMIS) $(CMOS)\n"
    package.package_name ;
  out "\tocamlfind ocamlc -a -package $(DEPENDS) -o $@ $(CMOS)\n" ;
  out "%%.cmo: %%.ml %%.cmi\n\
       \tocamlfind ocamlc -package $(DEPENDS) -c $<\n" ;
  out "%%.cmi: %%.mli\n\
       \tocamlfind ocamlc -package $(DEPENDS) -c $<\n" ;
  out "doc: *.mli\n\
       \tif [ ! -e doc ] ; then mkdir doc ; fi\n\
       \tocamlfind ocamldoc -html -d doc -package $(DEPENDS) $(MLIS)\n" ;
  out "install: all\n\
       \tocamlfind install %s META %s.cma $(CMIS)%s\n"
    package.package_name
    package.package_name
    (if js_present then " goji_jslib_" ^ package.package_name ^ ".zip" else "") ;
  out "uninstall:\n\
       \tocamlfind remove %s\n"
    package.package_name ;
  out "clean:\n\
       \t-rm *.cm* *~\n\
       \t-rm -rf doc\n" ;
  close_out fp_makefile 

(** Build the META file. *)
let generate_metas base_dir package =
  let open Goji_registry in
  let fp_meta = open_out (base_dir ^ "/META") in
  let out fmt = Printf.fprintf fp_meta fmt in
  let depends = compute_depends package in
  let rec sep s _ = function
    | [] -> ()
    | [e] -> out "%s" e
    | e :: es -> out "%s%s%a" e s (sep s) es
  in
  out "description = %S\n" package.package_doc ;
  out "requires = \"%a\"\n" (sep " ") depends ;
  out "version = %S\n" package.package_version ;
  out "archive(byte) = \"%s.cma\"\n" package.package_name ;
  out "exists_if = \"%s.cma\"\n" package.package_name ;
  close_out fp_meta

(** Builds the local JavaScript archive,
    returns false if no file has been generated *)
let grab_javascript_sources base_dir package =
  let open Goji_registry in
  (* obtain and format library *)
  let prev_wd = Sys.getcwd () in
  Sys.chdir base_dir ;
  let js_dir = "goji_jslib_" ^ package.package_name in
  let js_archive = "goji_jslib_" ^ package.package_name ^ ".zip" in
  (try
     Unix.mkdir js_dir 0o755 ;
   with Unix.Unix_error (err, _, _) ->
     Goji_messages.error "creating directory %S, %s"
       js_dir (Unix.error_message err)) ;
  iter_components
    (fun c -> match Goji_grab.run ~wd:js_dir c.grabber with
       | Goji_grab.Ok -> ()
       | Goji_grab.Failures fails ->
	 List.iter (Goji_messages.warning "%s") fails ;
	 Goji_messages.error
	   "Failed to obtain JavaScript sources for component %s.%s\n"
	   package.package_name c.name)
    package ;
  (* create archive (or purge if no file is present) *)
  let js_present = Sys.file_exists (js_dir ^ "/" ^ "goji_entry.js") in
  if js_present then
    Goji_messages.ensure
      (Sys.command ("zip -q -r " ^ js_archive ^ " " ^ js_dir ^ "/") = 0)
      "cannot create archive for %S, calling zip reported an error"
      package.package_name ;
  if Sys.command ("rm -rf " ^ js_dir) <> 0 then
    Goji_messages.warning
      "cannot remove temporary files for %S, calling rm reported an error"
      package.package_name ;
  Sys.chdir prev_wd ;
  js_present

(** Main entry point of the [generate] command *)
let main base_dir fix_case reorder_params event_backend packages  =
  List.iter
    (fun fn ->
      let open Dynlink in
      try loadfile (adapt_filename fn)
      with Error e -> Goji_messages.error "%s" (error_message e))
    packages ;
  if Hashtbl.length Goji_registry.package_store = 0 then
    Goji_messages.warning "empty package store, nothing to generate !" ;
  let open Goji_check_and_fix in
  check_params ~reorder_params () ;
  check_names ~fix_case () ;
  Goji_registry.iter_packages
    (fun package ->
      let package_base_dir = make_package_dir base_dir package in
      let js_present = grab_javascript_sources package_base_dir package in
      generate_sources package_base_dir package ;
      generate_makefiles package_base_dir package js_present ;
      generate_metas package_base_dir package)
