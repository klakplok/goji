(****************************************************************************)
(* GOJI (JavaScript Interface Generator for OCaml)                          *)
(* This file is published under the CeCILL licence                          *)
(* (C) 2013 Benjamin Canou                                                  *)
(****************************************************************************)

(** Internal package registry and functions to access it *)

(** A package is a group of components, compiled to a directory and an
    OCamlFind package *)
type package = {
  package_name : string ;
  (** The name of the OCamlFind package. *)
  package_doc : string ;
  (** The description of the OCamlFind package. *)
  package_version : string ;
  (** The version of the OCamlFind package. *)
  package_depends : string list ;
  (** The dependencies of the OCamlFind package. This field is there
      for convenience, for dependencies common to all components of
      the package. The effective list of dependencies is computed by
      unifying the dependencies of all components. *)
  mutable components : (string * component) list ;
  (** The map of components in the package. *)
}

(** A component is mapped to a toplevel OCaml compilation unit. *)
and component = {
  name : string ;
  (** The name of the OCaml module. *)
  version : string option ;
  (** The version number or string of the JavaScript library. *)
  binding_version : string option ;
  (** A version number or string. *)
  author : string option ;
  (** The author of the JavaScript library. *)
  binding_author : string option ;
  (** The author of the JavaScript library. *)
  license : Goji_license.t option ;
  (** A predefined or custom license from {!Goji_license}, this is the
      license of the generated OCaml code, but in case of inclusion of
      original comments or documentation, it has to be the same as the
      one of the JavaScript library. In any case, it is not a bad idea
      to choose the same license, so that people can see if they can
      use or not the binding without having to check the JavaScript
      sources. *)
  doc : string ;
  (** The documentation for the module, put a the beginning of the
      compilation unit, befor the meta information. *)
  grabber : Goji_grab.command ;
  (** A script that grabs the JavaScript sources. This script is
      executed inside a temporary, initially empty folder. Anything it
      puts inside this directory will be preserved iff a file named
      [goji_entry.js] is present after the execution of the
      script, otherwise it will be discarded. If more than one
      component has to grab JavaScript sources, scripts are run
      sequentially and can append code to the entry file. These files
      are installed along OCaml packages and then used by the [goji
      jslibs] command to produce the JavaScript sources for a specific
      set of packages. The final structure is a javascript file
      containing the concatenation of all entry files and all other
      auxiliary files merged. Be careful to use
      names that are not too prone to collisions.*)
  depends : string list ;
  (** The list of OCamlFind package dependencies. *)
  mutable elements : Goji_ast.binding list ;
  (** The component contents. *)
}


let package_store : (string, package) Hashtbl.t =
    Hashtbl.create 10

(** {2 Registering packages and components} *)

let register_package
    ?depends:(package_depends = [])
    ?doc:(package_doc = "UNDOCUMENTED")
    ~version:package_version
    package_name =
  let package =
    { package_name ;
      package_doc ;
      package_depends ;
      package_version ;
      components = [] }
  in
  Hashtbl.replace package_store package_name package ;
  package

let register_component
    ?(grabber = Goji_grab.nop)
    ?(depends = [])
    ?author ?version
    ?binding_author ?binding_version
    ?license
    ?(doc = "UNDOCUMENTED")
    package name elements =
  let component =
    { name; doc; grabber; depends; license;
      binding_author ; binding_version ;
      author; version ; elements }
  in
  package.components <- package.components @ [ name, component ] ;
  component

(** {2 Reading registered packages and components} *)

let iter_packages f =
  Hashtbl.iter (fun k v -> f v) package_store

let packages () =
  Hashtbl.fold (fun k _ r -> k :: r) package_store []

let iter_components f package =
  List.iter (fun (k, v) -> f v) package.components

let components package =
  fst (List.split package.components)

let lookup_package package_name =
  Hashtbl.find package_store package_name

let lookup_component package_name component_name =
  let package = Hashtbl.find package_store package_name in
  List.assoc package.components component_name
