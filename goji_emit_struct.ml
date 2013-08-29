(****************************************************************************)
(* GOJI (JavaScript Interface Generator for OCaml)                          *)
(* This file is published under the CeCILL licence                          *)
(* (C) 2013 Benjamin Canou                                                  *)
(****************************************************************************)

open Goji_ast
open Goji_pprint
open Goji_registry

class virtual emitter = object (self)
  method format_implementation component =
    self # format_header component
    ^^ hardline
    ^^ !^"module G = Goji_internal"
    ^^ hardline
    ^^ format_phrases (self # format_elements component.elements)
    ^^ hardline

  method format_interface component =
    self # format_header component
  ^^ hardline
  ^^ format_phrases (self # format_interface_elements component.elements)
  ^^ hardline

  method format_header { doc ; name ; version ; license ; author } =
    let text =
      self # format_doc (Doc doc)
      ^^ twice hardline
      ^^ self # format_doc
	(Doc ( "This Binding for the JavaScript library "
	       ^ name
	       ^ (match version with Some v -> " version " ^ v | None -> "")
	       ^ (match author with Some v -> " by " ^ v | None -> "")
	       ^ " has been automatically generated \
	           by the Goji binding generator. \
	           It should not be edited by hand."))
      ^^ twice hardline
      ^^ self # format_doc
	(Doc
	   (match license with
           | None -> "IT DOES NOT BEAR ANY LICENSING INFORMATION !"
	   | Some l -> "It is licensed under the " ^ l.Goji_license.long_name
	               ^ " (respecting the original library license). \
                          See the LICENSE file for more details."))
    in
    format_comment true text

  method format_doc = function
  | Doc text -> flow (break 1) (words text)
  | Nodoc -> empty

  method format_elements elements =
    List.flatten (List.map (self # format_element) elements)

  method format_interface_elements elements =
    List.flatten (List.map (self # format_interface_element) elements)

  method format_element = function
  | Structure (name, doc, elements) ->
    let doc = self # format_doc doc in
    let contents = format_phrases (self # format_elements elements) in
    [ format_module name ~doc contents ]
  | Section (text, elements) ->
    format_comment false (!^"{2 " ^^ !^text ^^ !^ "}")
    :: self # format_elements elements
  | Doc_block doc ->
    [ format_comment false (self # format_doc doc) ]
  | Group elements ->
    self # format_elements elements
  | Type (tparams, name, doc, type_mapping) ->
    self # format_type_definition tparams name doc type_mapping
  | Method (abbrv, name, params, body, ret, doc) ->
    self # format_method_definition abbrv name params body ret doc
  | Function (name, params, body, ret, doc) ->
    self # format_function_definition name params body ret doc
  | Exception (name, doc) ->
    let doc = self # format_doc doc in
    [ format_exception name ~doc [] ]
  | Inherits (n, t1, t2, doc) ->
    self # format_inherits_definition n t1 t2 doc

  method format_interface_element = function
  | Structure (name, doc, elements) ->
    let doc = self # format_doc doc in
    let contents = format_phrases (self # format_interface_elements elements) in
    [ format_module_sig name ~doc contents ]
  | Section (text, elements) ->
    format_comment false (!^"{2 " ^^ !^text ^^ !^ "}")
    :: self # format_interface_elements elements
  | Doc_block doc ->
    [ format_comment false (self # format_doc doc) ]
  | Group elements ->
    self # format_interface_elements elements
  | Type (tparams, name, doc, type_mapping) ->
    self # format_type_interface tparams name doc type_mapping
  | Method (abbrv, name, params, body, ret, doc) ->
    self # format_method_interface abbrv name params ret doc
  | Function (name, params, body, ret, doc) ->
    self # format_function_interface name params ret doc
  | Exception (name, doc) ->
    let doc = self # format_doc doc in
    [ format_exception name ~doc [] ]
  | Inherits (n, t1, t2, doc) ->
    self # format_inherits_interface n t1 t2 doc

  method virtual format_type_definition
      : (variance option * string) list -> string -> typedef -> comment
	-> document list
  method virtual format_method_definition
      : abbrv -> string -> parameter list -> body -> value -> comment
	-> document list
  method virtual format_function_definition
      : string -> parameter list -> body -> value -> comment
	-> document list
  method virtual format_inherits_definition
      : string -> abbrv -> abbrv -> comment
	-> document list

  method virtual format_type_interface
      : (variance option * string) list -> string -> typedef -> comment
	-> document list
  method virtual format_method_interface
      : abbrv -> string -> parameter list -> value -> comment
	-> document list
  method virtual format_function_interface
      : string -> parameter list -> value -> comment
	-> document list
  method virtual format_inherits_interface
      : string -> abbrv -> abbrv -> comment
	-> document list

end

let emit_implementation emitter fp component =
  ToChannel.pretty 0.9 80 fp (emitter # format_implementation component)

let emit_interface emitter fp component =
  ToChannel.pretty 0.9 80 fp (emitter # format_interface component)
