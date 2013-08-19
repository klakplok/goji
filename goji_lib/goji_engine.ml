(****************************************************************************)
(* GOJI (JavaScript Interface Generator for OCaml)                          *)
(* This file is published under the CeCILL licence                          *)
(* (C) 2013 Benjamin Canou                                                  *)
(****************************************************************************)

module SM = Map.Make (String)

(** {2 types} *)

type dependency =
| Package of string
| Component of string * string
| Extern of string

type meta = {
  name : string ;
  version : string option ;
  depends : dependency list ;
  author : string option ;
  license : Goji_license.t option ;
  doc : Goji_ast.comment ;
  grabber : Goji_grab.command ;
}

type component = {
  component_meta : meta ;
  elements : Goji_ast.structure_element list
}

type package = {
  package_meta : meta ;
  mutable components : component SM.t
}

let store = ref SM.empty

(** {2 Registering packages and components} *)

let register_package
    ?(grabber = Goji_grab.nop) ?(depends = []) ?author ?license ?version ?doc
    name =
  let doc = Goji_ast.(match doc with Some text -> Doc text | None -> Nodoc) in
  let package =
    { package_meta = { name; doc; grabber; depends; license; author; version } ;
      components = SM.empty }
  in
  store := SM.add name package !store ;
  package

let register_component
    ?(grabber = Goji_grab.nop) ?(depends = []) ?author ?license ?version ?doc
    package name elements =
  let doc = Goji_ast.(match doc with Some text -> Doc text | None -> Nodoc) in
  let component =
    { component_meta = { name; doc; grabber; depends; license; author; version }
    ; elements}
  in
  package.components <- SM.add name component package.components ;
  component

(** {2 Reading registered packages and components} *)

let packages () =
  snd (List.split (SM.bindings !store))

let components package =
  snd (List.split (SM.bindings package.components))

let lookup_package package =
  SM.find package !store

let lookup_component package component =
  SM.find component (SM.find package !store).components
