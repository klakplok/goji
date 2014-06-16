(****************************************************************************)
(* GOJI (JavaScript Interface Generator for OCaml)                          *)
(* This file is published under the CeCILL licence                          *)
(* (C) 2013 Benjamin Canou                                                  *)
(****************************************************************************)

(** The general purpose DSL of Goji, close to the AST. *)

(***************** keep comments in sync with goji_ast.ml *******************)

open Goji_syntax
open Goji_ast

(** {2 Some Concrete Syntax}

    This section introduces a few convenience parsers, taking a bribe
    of concrete syntax and building AST parts. They should only be
    used when combinators are not concise enough. String aliases are
    defined in order to recognize and differentiate parsed elements in
    the signatures of combinators. *)

(** The textual version of a type parameter. *)
type tparam = string

(** OCaml qualified identifier for values. *)
type ident = string

(** OCaml qualified identifier for types. *)
type tident = string

(** A JavaScript global path in pointed notation. *)
type jsglobal = string


(** Capitalize a string and replace forbidden characters with
    underscores so it becomes a valid constructor name *)
let mangle_constructor c =
  if c = "" then "Empty"
  else
    let c = match c.[0] with
      | 'A'..'Z' -> String.copy c
      | 'a'..'z' -> String.capitalize c
      | _ -> "C_" ^ c in
    for i = 1 to String.length c - 1 do
      match c.[i] with
      | 'a'..'z' | 'A'..'Z' | '\'' | '0'..'9' -> ()
      | _ -> c.[i] <- '_'
    done ; c

(** Uncapitalize a string and replace forbidden characters with
    underscores so it becomes a valid identifier name *)
let mangle_ident c =
  if c = "" then "empty"
  else
    let c = match c.[0] with
      | 'a'..'z' -> String.copy c
      | 'A'..'Z' -> String.uncapitalize c
      | _ -> "C_" ^ c in
    for i = 1 to String.length c - 1 do
      match c.[i] with
      | 'a'..'z' | 'A'..'Z' | '\'' | '0'..'9' -> ()
      | _ -> c.[i] <- '_'
    done ; c

(** Converts a textual type parameter to its AST representation. ["a"]
    is parsed as [(None, "a")], ["+b"] is parsed as [(Some Covariant,
    "b")] and ["-c"] is parsed as [(Some Contravariant, "c")].*)
let tparam (p : tparam) : Goji_ast.variance option * string = parse_type_variable p

(** Splits the module path and the variable / type identifier. For
    instance, [ident "Mod.Ule.var"] gives you
    [(["Mod";"Ule"],"var")]. *)
let ident (n : ident) = Goji_syntax.parse_qualified_name n
(** Same as {!ident} *)
let tident (n : tident) = Goji_syntax.parse_qualified_name n

(** Transforms the textual pointed notation of field accesses to a
    global to its AST version. ["g.f"] is parsed to [Field ("f",
    Global "b")]. *)
let jsglobal (str : jsglobal) = Goji_syntax.parse_js_global str

(** {2 Structure Elements} *)

(** Main structure element (by default, produces an OCaml module) *)
let structure name ?doc ctns =
  let doc = match doc with None -> Nodoc | Some text -> Doc text in
  Structure (name, doc, ctns)

(** Main structure element (by default, produces a big title in the doc) *)
let section name ctns =
  Section (name, ctns)

(** Produces a documentation block *)
let doc_block doc =
  Doc_block (Doc doc)

(** Makes a list of elements appear as one (useful when writing macros) *)
let group elts =
  Group elts


(** {2 Mappings} *)

(** Maps the [root] variable to a value of type [JavaScript.any]. *)
let any = Value (Any, Var "root")

(** Maps the [root] variable to a value of type [bool]. *)
let bool = Value (Bool, Var "root")

(** Maps the [root] variable to a value of type [int]. *)
let int = Value (Int, Var "root")

(** Maps the [root] variable to a value of type [string]. *)
let string = Value (String, Var "root")

(** Maps the [root] variable to a value of type [float]. *)
let float = Value (Float, Var "root")

(** Maps the [root] variable to a value of type [unit]. *)
let void = Value (Void, Var "root")

(** Maps the [root] variable to a specific OCaml type. *)
let abbrv ?(tparams = []) tname =
  let tpath = Goji_syntax.parse_qualified_name tname in
  Value (Abbrv ((tparams, tpath), Default), Var "root")

(** Maps the [root] variable to a specific OCaml type with a specific mapping. *)
let custom_abbrv ?(tparams = []) def tname =
  let tpath = Goji_syntax.parse_qualified_name tname in
  Value (Abbrv ((tparams, tpath), Custom def), Var "root")

(** Maps the [root] variable to a specific OCaml type with external converters. *)
let extern_abbrv ?(tparams = []) inj ext tname =
  let tpath = Goji_syntax.parse_qualified_name tname in
  Value (Abbrv ((tparams, tpath), Extern (inj, ext)), Var "root")

(** Maps the [root] variable to an array whose elements are mapped
    by the given argument. *)
let array v = Value (Array v, Var "root")

(** Maps the [root] variable to a list whose elements are mapped by
    the given argument. *)
let list v = Value (List v, Var "root")

(** Maps the [root] variable to an associative list whose values
    are mapped by the given argument. *)
let assoc v = Value (Assoc v, Var "root")

(** Maps the [root] variable to a value of a type parameter. *)
let param n =
  match parse_type_variable n with
  | None, n -> Value (Param n, Var "root")
  | _, _ -> invalid_arg (Printf.sprintf "Goji_dsl.param %S" n)

(** Maps the [root] variable to a tuple whose components are mapped as
    specified by the elements of the given list. The components are
    left as is, it is then up to you to reroot them separately using
    the {!(@@)} operator, otherwise they may collide. For instance,
    [tuple [ int ; float ]] describes a tuple whose two components are
    mapped to the root of the JavaScript value with different types,
    which is probably wrong. *)
let tuple vs = Tuple vs

(** Maps a JavaScript object to an OCaml record. To be used only in
    type definitions or custom type converters. Record fields can be
    written using the {!row} macro. *)
let record fields =
  Record fields

(** A shortcut for writing the elements of a record definition. *)
let row name ?doc def =
  let doc = match doc with None -> Nodoc | Some text -> Doc text in
  (name, def, doc)

(** Maps different forms of a JavaScript object to the cases of an
    OCaml sum type. To be used only in type definitions or custom type
    converters. Cases can be written using the {!constr} macro. *)
let variant cases =
  Variant cases

(** A shortcut for writing the cases of a sum type definition. *)
let constr name guard ?doc defs =
  let doc = match doc with None -> Nodoc | Some text -> Doc text in
  (name, guard, defs, doc)

(** Maps an OCaml function to a JavaScript one. For injection, some
    combinations of parameters will be rejected, in particular
    optional arguments with too complex guards or the use of
    [rest]. *)
let callback params ret =
  Value (Callback (params, ret), Var "root")

(** A functional value meant to be an event handler. See {!callback}
    case for an explanation of the last two parameters. Usable only in
    parameters. See {!auto} and {!canceller} for the [canceller]
    parameter. *)
let event ?(canceller = Manual_canceller) params ret =
  Value (Handler (params, ret, canceller), Var "root")

(** A fixed list of tags phantom type. To be used only in type
    parameters.  Compiled to a fixed polymorphic variant type. *)
let tags ls = Tags (ls, Some [])

(** A list of tags phantom type. To be used only in type parameters.
    Compiled to an open polymorphic variant type [[> .. ]]. *)
let tags_min ls = Tags (ls, None)

(** A list of tags phantom type. To be used only in type parameters.
    Compiled to a bounded polymorphic variant type [[< .. ]]. *)
let tags_max ls = Tags ([], Some ls)

(** A list of tags phantom type. To be used only in type parameters.
    Compiled to a fixed polymorphic variant type. *)
let tags_range req pos = Tags (req, Some pos)

(** Wraps the code to cancel an event handler. How this code is made
    available depends on the event handling backend.  The canceller
    [body] is generated after the body of the binding. In this
    context, the [handler] variable points to the generated JavaScript
    function and the [result] variable points to the result of the
    main body. Both can be useful since some JavaScript library need
    the original function to unregister an event handler, while some
    others need some identifier returned by the registration
    function. All variables from top level [Abs] bindings are also
    available. *)
let canceller body =
  Canceller body

(** Asks the event handling backend to insert some fake cancellation
    mechanism, for when the library does not provide one. Not always a
    good idea. *)
let auto =
  Auto_canceller

(** A specific DSL to write JavaScript constants. *)
module Const = struct
  let nan = Const_NaN
  let int i = Const_int i
  let float f = Const_float f
  let bool b = Const_bool b
  let string s = Const_string s
  let undefined = Const_undefined
  let null = Const_null
  let obj cstr = Const_object cstr
end

(** A specific DSL to write guards as infix boolean expressions. *)
module Guard = struct
  include Const
  let (<>) s c = Not (Const (s, c))
  let (=) s c = Const (s, c)
  let (!=) s s' = Not (Equals (s, s'))
  let (==) s s' = Equals (s, s')
  let not g = Not g
  let (&&) gl gr = And (gl, gr)
  let (||) gl gr = Or (gl, gr)
  let tt = True
  let ff = False
  let raise ?(path = []) ex = Raise (path, ex)
end

(** {2 JavaScript storage descriptors} *)

(** The predefined [root] variable, mapped to the root of the JavaScript value in
    a type definition, the return value in functions, methods and
    callback definitions, and the elements of collections (arrays,
    lists ans assocs). *)
let root = Var "root"

(** The predefined [this] variable, mapped to the receiver object in
    method bindings. *)
let this = Var "this"

(** A custom variable. *)
let var n = Var n

(** A global JavaScript variable. *)
let global str = jsglobal str

(** A name field accessor in a JavaScript object. *)
let field root n = Field (root, Volatile (Const_string n))

(** An index field accessor in a JavaScript array. *)
let cell root n = Field (root, Volatile (Const_int n))

(** A generic field accessor in a JavaScript object. *)
let acc root n = Field (root, n)

(** A positional argument, by default of the call site [args]. Write
    only in bodies, read only in callbacks. *)
let arg ?(site = "args") n = Arg (site, n)

(** An optional argument, pushed at the end of positional arguments. *)
let rest ?(site = "args") () = Rest (site)

(** A JavaScript array considered as a sequence of optional arguments,
    all pushed at the end of positional arguments. *)
let unroll ?(site = "args") () = Unroll (site)

(**/**)
let rec reroot v ns =
  match v with
  | Tuple vs ->
    Tuple (List.map (fun v -> reroot v ns) vs)
  | Option (g, v) ->
    Option (reroot_guard g ns, reroot v ns)
  | Value (l, s) ->
    Value (l, reroot_storage s ns)
  | Record fields ->
    Record (List.map
              (fun (n, v, c) ->
                (n, reroot v ns, c))
              fields)
  | Variant cases ->
    Variant (List.map
               (fun (n, g, vs, c) ->
                 (n, reroot_guard g ns,
		  List.map (fun v -> reroot v ns) vs, c))
               cases)
  | Tags _ as c -> c
and reroot_storage sto ns =
  match sto with
  | Var "root" -> ns (* all this code for this... *)
  | Field (s, s') -> Field (reroot_storage s ns, reroot_storage s' ns)
  | Volatile _ | Var _ | Arg _ | Rest _ | Unroll _ | Global _  as s -> s
and reroot_guard g ns =
  match g with
  | Const (s, c) -> Const (reroot_storage s ns, c)
  | Equals (s, s') -> Equals (reroot_storage s ns, reroot_storage s' ns)
  | Not g -> Not (reroot_guard g ns)
  | And (gl, gr) -> And (reroot_guard gl ns, reroot_guard gr ns)
  | Or (gl, gr) -> Or (reroot_guard gl ns, reroot_guard gr ns)
  | Raise _ | True | False as const -> const
(**/**)

(** By default, all aforedefined mappings use the [root] variable as
    JavaScript location. This operator relocates the root of a mapping
    to a given location. For instance, to speak of an array of integer
    located in the field "nb" of a JavaScript global "x", one can
    write [array int @@ jsglobal "x.nb"]. As a more complex example
    showing a recursive use of [(@@)], [tuple [ int @@ field root "x"
    ; bool @@ field root "t" ] @@ field root "b"] will map a
    JavaScript object [{b: {x: 3, t: false}}] to the OCaml value [(3,
    false)].
*)
let (@@) = reroot

(** {2 Predefined Mappings} *)

(** Maps the consecutive cells of a JavaScript array to the components
    of an OCaml tuple. [tuple_cells [ int : float ]] is equivalent to
    [tuple [ int @@ cell root 0 ; float @@ cell root 1 ]].*)
let tuple_cells vs =
  Tuple (List.mapi (fun n v -> v @@ cell root n) vs)

(** Maps named attributes of a JavaScript object to the components of
    an OCaml tuple. [tuple_fields [ "i", int : "f", float ]] is equivalent to
    [tuple [ int @@ field root "i" ; float @@ field root "f" ]].*)
let tuple_fields vs =
  Tuple (List.map (fun (f, v) -> v @@ field root f) vs)

(** Maps an [undefined] JavaScript value to the [None] case and maps
    the [Some] case using the given mapping parameter. *)
let option_undefined def =
  Option (Guard.(var "root" = undefined), def)

(** Maps a [NaN] JavaScript value to the [None] case and maps
    the [Some] case using the given mapping parameter. *)
let option_nan def =
  Option (Guard.(var "root" = nan), def)

(** Maps a [null] JavaScript value to the [None] case and maps the
    [Some] case using the given mapping parameter. *)
let option_null def =
  Option (Guard.(var "root" = null), def)

(** Map a simple JavaScript int enum as a sum type  *)
let int_enum cases =
  let cases = List.map
      (fun (n, i) -> constr n Guard.(root = int i) [])
      cases
  in
  variant cases
                   
(** Map a simple JavaScript string enum as a sum type  *)
let string_enum cases =
  let cases = List.map
      (fun (n, s) -> constr n Guard.(root = string s) [])
      cases
  in
  variant cases

(** Map a simple JavaScript string enum as a sum type directly using
    the (mangled) strings as constructor names. *)
let simple_string_enum cases =
  let cases = List.map
    (fun n -> constr (mangle_constructor n) Guard.(root = string n) [])
    cases
  in
  variant cases

let nonempty_array_or_undefined ty =
  let injector = ([ "Goji_internal" ], "inject_nonempty_array_or_undefined") in
  let extractor = ([ "Goji_internal" ], "extract_nonempty_array_or_undefined") in
  Value (Abbrv (([ ty ], ([], "array")), Extern (injector, extractor)), Var "root")

let nonempty_list_or_undefined ty =
  let injector = ([ "Goji_internal" ], "inject_nonempty_list_or_undefined") in
  let extractor = ([ "Goji_internal" ], "extract_nonempty_list_or_undefined") in
  Value (Abbrv (([ ty ], ([], "list")), Extern (injector, extractor)), Var "root")

let nonempty_array_or_null ty =
  let injector = ([ "Goji_internal" ], "inject_nonempty_array_or_null") in
  let extractor = ([ "Goji_internal" ], "extract_nonempty_array_or_null") in
  Value (Abbrv (([ ty ], ([], "array")), Extern (injector, extractor)), Var "root")

let nonempty_list_or_null ty =
  let injector = ([ "Goji_internal" ], "inject_nonempty_list_or_null") in
  let extractor = ([ "Goji_internal" ], "extract_nonempty_list_or_null") in
  Value (Abbrv (([ ty ], ([], "list")), Extern (injector, extractor)), Var "root")

(** {2 Value mapping bodies} *)

(** Calls a method on an object. By default, the call site is [args],
    consistently with the {!arg} and {!rest}, so yu do not need to
    precise it if you have only one call site. *)
let call ?(site = "args") sto =
  Call (sto, site)

(** Executes a body within a try catch block and turns JavaScript
    exceptions into constants. These constants can then be matched by
    a guard, for instance to turn them into cases of an OCaml variant
    when extracting the result. The [guard * const] list associates
    exception types (encoded as guards, since anything can be thrown
    in JavaScript) to their resulting constants. *)
let try_catch ?(exns = [ Const (Var "root", Const_bool true), Const_undefined ]) body =
  Try (body, exns)

(** Calls the JavaScript [new] operator. See {!call} for more
    information. *)
let call_constructor ?(site = "args") sto =
  New (sto, site)

(** Calls a method on an object. By default, the call site is [args]
    and the receiver object is the [this] variable, consistently with
    the rest of the DSL. In particular, you do not need to precise
    them to use [call_method] in conjunction with {!arg} and
    {!def_method}. *)
let call_method ?(site = "args") ?(sto = Var "this") name =
  Call_method (sto, name, site)

(** Returns the value at a given JavaScript location. *)
let get sto = Access sto

(** Returns a JavaScript constant. *)
let get_const sto = Access (Volatile sto)

(** An assignment of a JavaScript constant to a given location. *)
let set_const dst v = Set (dst, Volatile v)

(** An imperative assignment to a location from another. *)
let set dst src = Set (dst, src)

(** A [let  .. in] construct. *)
let abs name v body = Abs (name, v, body)

(** A [let _ = .. in let _ = .. in ..] construct. *)
let rec seq = function
  | [] -> invalid_arg "Goji_dsl.seq"
  | [ b ]  -> b
  | b :: bs -> Abs ("_", b, seq bs)

(** An [if .. then .. else] construct using a guard as condition. This
    is also the only possibility to raise an exception.  *)
let test guard bt bf = Test (guard, bt, bf)

(** An [if .. then] construct using a guard as condition. *)
let test' guard bt = Test (guard, bt, Nop)

(** Returns the value of the variable, or raises [Invalid_argument "instanceof name"]. *)
let var_instanceof name cstr =
  test Guard.(var name = obj cstr
              || raise ("Invalid_argument \"instanceof " ^ cstr ^ "\""))
    (get (var name)) (get (var name))

(** Returns the concatenation of some body expressions, undefined are dropped *)
let concat_strings exprs =
  let rec concat = function
    | [] -> call ~site:"concat" (jsglobal "String.concat")
    | e :: es ->
      abs "_"
        (abs "tmp" e
           (test Guard.(var "tmp" <> undefined)
              (set (rest ~site:"concat" ()) (var "tmp"))
              Nop))
        (concat es)
  in concat exprs

(** Returns the concatenation of some variables, undefined are dropped *)
let concat_string_vars vars =
  let rec concat = function
    | [] -> call ~site:"concat" (jsglobal "String.concat")
    | v :: vs ->
      abs "_"
        (test Guard.(var v <> undefined)
           (set (rest ~site:"concat" ()) (var v))
           Nop)
        (concat vs)
  in concat vars

(** {2 Bindings} *)

let def_type ?(tparams = []) ?doc tname tdef =
  let tparams = List.map parse_type_variable tparams in
  let doc = match doc with None -> Nodoc | Some text -> Doc text in
  Type (tparams, tname, tdef, doc)

let def_constructor ?(tparams = []) tname name ?doc params body =
  let doc = match doc with None -> Nodoc | Some text -> Doc text in
  Function (name, params, body, abbrv ~tparams tname, doc)

let def_method ?(tparams = []) tname name ?doc params body ret =
  let doc = match doc with None -> Nodoc | Some text -> Doc text in
  let tpath = Goji_syntax.parse_qualified_name tname in
  Method ((tparams, tpath), name, params, body, ret, doc)

let def_function name ?doc params body ret =
  let doc = match doc with None -> Nodoc | Some text -> Doc text in
  Function (name, params, body, ret, doc)

let def_value name ?doc body ret =
  let doc = match doc with None -> Nodoc | Some text -> Doc text in
  Let (name, body, ret, doc)

let inherits (tp1, (t1 : tident)) (tp2, (t2 : tident)) ?doc name =
  let doc = match doc with None -> Nodoc | Some text -> Doc text in
  Inherits (name,
            (tp1, Goji_syntax.parse_qualified_name t1),
            (tp2, Goji_syntax.parse_qualified_name t2),
            doc)


(** {2 Type Definitions} *)

let abstract def = Typedef (Abstract, def)
let public def = Typedef (Public, def)
let gen_sym = Gen_sym
let gen_id = Gen_sym
let format fmt = Format

(** {2 One-To-One Bindings}

    These are simplified binding combinators provided for
    convenience. They are to be used when mapping OCaml entities to
    their direct JavaScript equivalents. *)

(** Maps a fresh OCaml type to abstract JavaScript values.

    Example call: [map_type ~tparams ~doc tname]
    
    @param tname     The name of the OCaml type.
    @param doc       The name OCamlDoc description.
    @param tparams   The parameters of the OCaml type, as processed by {!tparam}. *)
let map_type ?(tparams : tparam list = []) ?doc (tname : tident) =
  let tparams = List.map parse_type_variable tparams in
  let doc = match doc with None -> Nodoc | Some text -> Doc text in
  Type (tparams, tname, abstract any, doc)

(** Maps a JavaScript function to an OCaml function.

    Example call: [map_function name ~doc params js_path ret]

    @param name     The name of the OCaml function.
    @param doc      The name OCamlDoc description.
    @param js_path  The location of the JavaScript function, processed as by {!global}.
    @param params   The description of the OCaml parameters. The
                    JavaScript function call is the default [args] one.
    @param ret      The description of the return value. The JavaScript return
                    value is bound to the default variable [root]. *)
let map_function name ?doc params (js_path : jsglobal) ret =
  let body = Call (global js_path, "args") in
  def_function name ?doc params body ret

let map_constructor
    ?(tparams = []) (tname : tident)
    (name : ident) ?doc params (js : jsglobal) =
  let body = New (jsglobal js, "args") in
  def_constructor ~tparams tname name ?doc params body

let map_method
    ?tparams (tname : tident)
    jname ?rename ?doc params ret =
  let name = match rename with None -> jname | Some n -> n in
  def_method ?tparams tname name ?doc params (call_method jname) ret

(** Maps a JavaScript attribute value to a getter and (optionally a
    setter) function(s). By default, the JavaScript name is used,
    unless the [rename] parameter is passed. The getter is named
    [name / rename] and the setter [set_name / set_rename]. The last
    argument is a [value] description in which the [root] variable
    points to the JavaScript attribute. *)
let map_attribute
    ?tparams (tname : tident)
    jname ?rename ?doc ?(read_only = false) def =
  let name = match rename with None -> jname | Some n -> n in
  group
    ([ def_method ?tparams tname name ?doc [] (get (field this jname)) def ]
     @ (if read_only then []
       else [ def_method ?tparams tname ("set_" ^ name) ?doc
                [ Curry, "value", Nodoc, (def @@ field this jname) ]
                Nop void ]))

(** Maps a JavaScript global value to a getter and (optionally a
    setter) function(s). The getter is named after the parameter
    [name] and the setter [set_name]. The last argument is a [value]
    description whose [mapping]s should only reference global
    JavaScript storages. *)
let map_global name ?doc ?(read_only = false) def =
  group
    ([ def_function name ?doc [] Nop def ]
     @ (if read_only then []
       else [ def_function ("set_" ^ name) ?doc
                [ Curry, "value", Nodoc, def ]
                Nop void ]))

(** {2 Arguments} *)

(** An OCaml positional argument. The name is the one used in the
    implementation and in the generated documentation. *)
let curry_arg name ?doc descr =
  let doc = match doc with None -> Nodoc | Some text -> Doc text in
  Curry, name, doc, descr

(** An OCaml optional argument. The name is the OCaml label. *)
let opt_arg name ?doc descr =
  let doc = match doc with None -> Nodoc | Some text -> Doc text in
  Optional, name, doc, descr

(** An OCaml labeled argument. The name is the OCaml label. *)
let labeled_arg name ?doc descr =
  let doc = match doc with None -> Nodoc | Some text -> Doc text in
  Labeled, name, doc, descr
