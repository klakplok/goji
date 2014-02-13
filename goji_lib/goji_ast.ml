(****************************************************************************)
(* GOJI (JavaScript Interface Generator for OCaml)                          *)
(* This file is published under the CeCILL licence                          *)
(* (C) 2013 Benjamin Canou                                                  *)
(****************************************************************************)

(** The public AST of Goji bindings.*)

(***************** keep comments in sync with goji_dsl.ml *******************)

(** {2 AST Types} *)

(** The type of documentation strings. *)
type comment =
  | Doc of string
  (** Verbatim doc string *)
  | Nodoc
  (** This case should never be used*)

(** Qualified path to an OCaml value / type. *)
and path = string list * string

(** Instance of an OCaml type. *)
and abbrv = value list * path

(** Basic type of the element tree, both for structure elements and bindings. *)
and binding =
  | Structure of string * comment * binding list
  (** Produces an OCaml module. *)
  | Section of string * binding list
  (** Produces a section in the doc. *)
  | Doc_block of comment
  (** Produces a comment block in the doc. *)
  | Group of binding list
  (** Groups several elements as one, useful for writing macros that expand to more than one element. *)
  | Type of (variance option * string) list * string * typedef * comment
  (** Maps an OCaml type definition to a specific JavaScript construct. *)
  | Function of string * parameter list * body * value * comment
  (** Maps an OCaml function. In the [value] mapping, the [root]
      variable points to the root of the result of the [body]. *)
  | Let of string * body * value * comment
  (** Maps an OCaml value. In the [value] mapping, the [root] variable
      points to the root of the result of the [body]. *)
  | Method of abbrv * string * parameter list * body * value * comment
  (** Maps an OCaml function attached to a specific type. In the
      [value] mapping, the [root] variable points to the root of the
      result of the [body]. In all mappings, the [this] variable
      points to the root of the (injected) value of type [abbrv]. *)
  | Exception of string * comment
  (** Produces a new exception that can be used with the Raise construct. *)
  | Inherits of string * abbrv * abbrv * comment
  (** Make the first type a subtype of the second (prodces a named
      corecion function). *)

(** Variance of OCaml type parameters (when not specified, no
    annotation is produced). *)
and variance =
  | Covariant
  (** +'a *)
  | Contravariant
  (** -'a *)

(** Describes the content of a type definition. *)
and typedef =
  | Typedef of visibility * value
  (** A concrete OCaml type definition, including records and
      variants. In the [value] mapping, the [root] variable points to
      the root of the JavaScript value. *)
  | Gen_sym
  (** A (hidden) unique string type, generates a make_<typename>
      function. *)
  | Gen_id
  (** a (hIdden) unique integer type, generates a make_<typename>
      function. *)
  | Format
  (** TODO. *)

(** Describes how type definitions are exported. *)
and visibility =
  (** The structure of the type will appear in the interface
      e.g. ["type t = float * float"]. *)
  | Public
  (** The structure of the type will appear in the interface with a
      private annotation e.g. ["type t = private float * float"]. *)
  | Private
  (** The type will be completely abstract e.g. ["type t"]. *)
  | Abstract

(** The value type describes a mapping between en OCaml type and a
    JavaScript structure. It is a reversible mapping (a sort of lens),
    meaning that a single mapping is used both to extract an OCaml
    value from a JavaScript context, or to inject an OCaml value in a
    JavaScript context.  This very type mostly describes the toplevel
    OCaml structure, the mappings to JavaScript are described by the
    Value case. *)
and value =
  | Value of mapping * storage
  (** Maps an OCaml sub-value to the accessor of its JavaScript
      counterpart by describing where it is located in the JavaScript
      context (type {!storage}) and how to perform the conversion
      (type {!mapping}). *)
  | Tuple of value list
  (** Groups several mappings bindings as the components of an OCaml
      tuple. *)
  | Option of guard * value
  (** Associate two possible mappings to an OCaml option type using a
      {!guard}. The [guard] corresponds to the [None] case. *)
  | Record of (string * value * comment) list
  (** Wraps several mappings as the fields of an OCaml record (only
      allowed in type definitions). *)
  | Variant of (string * guard * value list * comment) list
  (** Associates different mappings to the cases of an OCaml sum
      type. When converting from JavaScript, the first case whose
      {!guard} is true is selected. *)
  | Tags of string list * variance option
  (** A list of tags using polymorphic variants.
      To be used only as phantom flags. *)

(** The body of a function / method binding. *)
and body = 
  | Call of storage * call_site
  (** Calls a function at some JavaScript location and returns its
      result, introduces a name for this call site to be used in
      JavaScript storages. *)
  | Call_method of storage * string * call_site
  (** Calls a method at some JavaScript location and returns it s
      result. *)
  | Try of body * (guard * const) list
  (** Executes a body within a try catch block and turns JavaScript
      exceptions into constants. These constants can then be matched
      by a guard, for instance to turn them into cases of an OCaml
      variant when extracting the result. The [guard * const] list
      associates exception types (encoded as guards, since anything
      can be thrown in JavaScript) to their resulting constants. *)
  | New of storage * call_site
  (** Calls a constructor at some JavaScript location and returns the
      allocated object. *)
  | Access of storage
  (** Accesses a JavaScript location, to be used as the first
      subexpression of [Abs] or as return value. *)
  | Inject of path * value
  (** Inject the value in an OCaml variable in the JavaScript context. *)
  | Set of storage * storage
  (** Stores the current content of a JavaScript location to anotther
      one, returns a void result, useful for writing complex setters
      (with intermediate objects) or insert phantom parameters in call
      sites. *)
  | Test of guard * body * body
  (** Evaluates a guard and chooses a code path depending on its
      result. Can also be used to raise an exception depending (or
      not) on the environment. *)
  | Abs of string * body * body
  (** A [let ... in] like structure, useful to compose operations. *)
  | Nop
  (** An empty body, useful for writing dummy functions or setters. *)

(** The type of reversible guards. When converting from JavaScript to
    OCaml, guards are evaluated to discriminate between cases in
    options and enums. When converting the other way, [Const] leaves
    are injected into the JavaScript context (unless under a [Not]
    operator), other operations are ignored. By default, the result is
    [undefined]. *)
and guard =
  | Const of storage * const
  (** Check / inject a constant in the JavaScript context. *)
  | Equals of storage * storage
  (** Check that two elements are equal in the JavaScript context.
      When converting from OCaml to JavaScript, assigns the left part
      of the pair to the left one. *)
  | Not of guard
  (** Invert the result of a guard. When injecting, the subexpression
      is ignored.*)
  | And of guard * guard
  (** Sequential [(&&)] operation. Both subexpression are injected
      when converting from OCaml to JavaScript. *)
  | Or of guard * guard
  (** Sequential [(||)] operation. Only the first subexpression is
      injected when converting from OCaml to JavaScript. *)
  | Raise of path
  (** Raises an OCaml exception when converting from JavaScript to
      OCaml. Can be used at toplevel when a specific case of a sum
      type cannot happen, but should probably used as the second
      parameter of a sequential operator. For instance, use the
      following to raise [Not_found] is the result of a function is
      null:

      [And (Const (Var "root", Const_null), Raise ([], "Not_found"))]. *)
  | True
  (** Forces the evaluation to [true]. *)
  | False
  (** Forces the evaluation to [false]. *)

(** The name of a call site. *)
and call_site = string

(** A unique identifier to link event registration and cancellation
    when that makes sense. *)
and event_name = string option

(** Low level description of mappings (the JavaScript path part). *)
and storage =
  | Global of string
  (** A global JavaScript variable. *)
  | Var of string
  (** A Goji variable *)
  | Arg of call_site * int
  (** A positional arg of a call site, write only except in callbacks. *)
  | Rest of call_site
  (** An optional argument pushed after all other positional
      arguments and already pushed optional arguments. *)
  | Unroll of call_site
  (** Makes sense (only) when associated with a JavaScript
      array. Unrolls the collection after all other positional
      arguments and already pushed optional arguments. *)
  | Field of storage * storage
  (** A field in a JavaScript object. *)
  | Volatile of const
  (** A JavaScript constant, useful for indexing or to create values on the fly. *)

(** Low level description of mappings. *)
and mapping =
  | Int
  (** Maps JavaScript Numbers to OCaml ints. *)
  | Float
  (** Maps JavaScript Numbers to OCaml floats. *)
  | String
  (** Maps JavaScript strings to OCaml ones. If you want to use
      JavaScript strings, use [Abbrv ([ "JavaScript" ], "string")].*)
  | Bool
  (** Maps JavaScript booleans to OCaml ones. *)
  | Any
  (** Maps any kind of JavaScript object to an abstract OCaml value of
      type [Abbrv ([ "JavaScript" ], "any")]. *)
  | Void
  (** Maps useless JavaScript values either to unit or nothing
      depending on the case. *)
  | Abbrv of abbrv * converters
  (** An OCaml type abbreviation and how to inject / extract it. *)
  | Array of value
  (** A mapping between and an OCaml array and a JavaScript list. The
      value parameter maps each element of the OCaml array to the
      corresponding one in the JavaScript array. In the mapping of
      elements, the [root] variable describes the root of each
      element, hiding the [root] of the surrounding value mapping. *)
  | List of value
  (** Same a Array but with an OCaml list. *)
  | Assoc of value
  (** Maps an OCaml associative list from strings to values to a
      JavaScript objects seen as an hashmap. As with [Array], the
      [root] variable points to the root of each element. *)
  | Param of string
  (** An OCaml type parameter (without any conversion). *)
  | Callback of parameter list * value
  (** A functional value. Inside the mappings of arguments, the
      [call_site] of the callback is named [args] and is the only
      authorized one. Inside the mapping of the return value, the
      [root] variable points to the return value. Not all combinations
      of argument types are accepted for injection. *)
  | Handler of parameter list * value * canceller
  (** A functional value meant to be an event handler. See [Callback]
      case for an explanation of the first two parameters and
      {!canceller} *)

(** The type of event cancellation methods. *)
and canceller =
  | Manual_canceller
  (** Does nothing special about cancelling. *)
  | Auto_canceller
  (** Asks the event handling backend to insert some fake cancellation
      mechanism, for when the library does not provide one. Not always
      a good idea. *)
  | Canceller of body
  (** Provides the complete code to cancel an event handler. How this
      code is made available depends on the event handling backend.
      The canceller [body] is generated after the body of the
      binding. In this context, the [handler] variable points to the
      generated JavaScript function and the [result] variable points
      to the result of the main body. Both can be useful since some
      JavaScript library need the original function to unregister an
      event handler, while some others need some identifier returned
      by the registration function. All variables from top level [Abs]
      bindings are also available. *)

(** Constants, used to implement phantom arguments and in guards. *)
and const =
  | Const_int of int
  | Const_float of float
  | Const_NaN
  | Const_bool of bool
  | Const_string of string
  | Const_undefined
  | Const_object of string
  | Const_null

(** describe a single function paramerer, used in top level mappings and callbacks . *)
and parameter =
  parameter_type * string * comment * value
and parameter_type =
  | Optional
  (** produces a proper OCaml named optional argument. *)
  | Curry
  (** produces a normal OCaml argument. *)
  | Labeled
  (** produces an OCaml named argument. *)

(** Used to specify how values are converted. *)
and converters =
  | Default
  (** use default_converters inject_<t> and extract_<t>. *)
  | Custom of value
  (** generate ad-hoc converters using a Goji spec. *)
  | Extern of path * path
  (** external OCaml converter functions. *)

(** {2 AST Utilities} *)

(** Generic functional iteration over an AST, produces a deep copy of
    the tree. Implemented as an object to customize operations on one
    of the AST types without having to rewrite boilerplate copying
    code for the others. Just inherit from [map] and redefine the
    method corresponding to the type of interest. *)
class map = object (self)
  method comment = function
    | Doc s -> Doc s
    | Nodoc -> Nodoc

  method binding = function
    | Structure (s, c, sel) ->
      Structure (s, c, List.map (self # binding) sel)
    | Section (s, sel) ->
      Section (s, List.map (self # binding) sel)
    | Doc_block c ->
      Doc_block c
    | Group sel ->
      Group (List.map (self # binding) sel)
    | Type (pl, s, tm, doc) ->
      Type (pl, s, self # typedef tm, self # comment doc)
    | Method (abbrv, s, pl, body, ret, doc) ->
      Method (self # abbrv abbrv, s,
	      List.map (self # parameter) pl,
	      self # body body,
	      self # value ret,
	      self # comment doc)
    | Function (s, pl, body, ret, doc) ->
      Function (s, List.map (self # parameter) pl,
		self # body body,
		self # value ret,
		self # comment doc)
    | Let (s, body, ret, doc) ->
      Let (s,
	   self # body body,
	   self # value ret,
	   self # comment doc)
    | Exception (s, doc) ->
      Exception (s, self # comment doc)
    | Inherits (n, abbrv1, abbrv2, doc) ->
      Inherits (n, self # abbrv abbrv1, self # abbrv abbrv2, self # comment doc)

  method variance (v : variance) = v

  method typedef = function
    | Typedef (vis, v) -> Typedef (vis, self # value v)
    | Gen_sym -> Gen_sym
    | Gen_id -> Gen_id
    | Format -> Format

  method visibility (vis : visibility) = vis

  method value = function
    | Tuple v -> Tuple (List.map (self # value) v)
    | Option (g, v) ->
      Option (self # guard g, self # value v)
    | Value (l, s) ->
      Value (self # mapping l, self # storage s)
    | Record fields ->
      Record (List.map
		(fun (s, v, doc) ->
		   (s, self # value v, self # comment doc))
		fields)
    | Variant cases ->
      Variant (List.map
		 (fun (s, g, v, doc) ->
		    (s, self # guard g, List.map (self # value) v, self # comment doc))
		 cases)
    | Tags (ns, v) -> Tags (ns, v)

  method body = function 
    | Call_method (s, n, cs) ->
      Call_method (self # storage s, n, self # call_site cs)
    | Call (s, cs) ->
      Call (self # storage s, self # call_site cs)
    | Try (b, l) ->
      Try (self # body b, List.map (fun (s, c) -> self # guard s, self # const c) l)
    | New (s, cs) ->
      New (self # storage s, self # call_site cs)
    | Access s ->
      Access (self # storage s)
    | Set (s, s') ->
      Set (self # storage s, self # storage s')
    | Inject (n, v) ->
      Inject (n, self # value v)
    | Abs (s, b1, b2) ->
      Abs (s, self # body b1, self # body b2)
    | Test (g, b1, b2) ->
      Test (self # guard g, self # body b1, self # body b2)
    | Nop -> Nop

  method guard = function
    | Const (s, c) -> Const (self # storage s, self # const c)
    | Equals (s, s') -> Equals (self # storage s, self # storage s')
    | Not g -> Not (self # guard g)
    | And (g1, g2) -> And (self # guard g1, self # guard g2)
    | Or (g1, g2) -> Or (self # guard g1, self # guard g2)
    | True | False as c -> c
    | Raise p -> Raise p

  method call_site (s : call_site) = s

  method abbrv (a : abbrv) = a

  method storage = function
    | Global s -> Global s
    | Var s -> Var s
    | Unroll cs -> Unroll cs
    | Arg (cs, i) -> Arg (self # call_site cs, i)
    | Rest c -> Rest (self # call_site c)
    | Field (s, n) -> Field (self # storage s, n)
    | Volatile c -> Volatile (self #const c)

  method mapping = function
    | Int | String | Bool | Float | Any | Void as t -> t
    | Abbrv (abbrv, converters) ->
      Abbrv (self # abbrv abbrv, converters)
    | Array v -> Array (self # value v)
    | List v -> List (self # value v)
    | Assoc v -> Assoc (self # value v)
    | Param string -> Param string
    | Callback (pl, v) ->
      Callback (List.map (self # parameter) pl, self # value v)
    | Handler (pl, v, Canceller b) ->
      Handler (List.map (self # parameter) pl, self # value v,
               Canceller (self # body b))
    | Handler (pl, v, c) ->
      Handler (List.map (self # parameter) pl, self # value v, c)


  method const = function
    | Const_int i -> Const_int i
    | Const_float f -> Const_float f
    | Const_bool b -> Const_bool b
    | Const_string s -> Const_string s
    | Const_undefined -> Const_undefined
    | Const_object cstr -> Const_object cstr
    | Const_null -> Const_null
    | Const_NaN -> Const_NaN

  method parameter (pt, n, doc, v) =
    (pt, n, self # comment doc, self # value v)

  method converters = function
    | Default -> Default
    | Custom v -> Custom (self # value v)
    | Extern (p1, p2) -> Extern (p1, p2)
end

(** Generic imperative iteration over an AST. Implemented as an object
    to customize operations on one of the AST types without having to
    rewrite boilerplate copying code for the others. Just inherit from
    [iter] and redefine the method corresponding to the type of
    interest. *)
class iter = object (self)
  method comment = function
    | Doc s -> ()
    | Nodoc -> ()

  method binding = function
    | Structure (s, c, sel) ->
      List.iter (self # binding) sel
    | Section (s, sel) ->
      List.iter (self # binding) sel
    | Doc_block c -> ()
    | Group sel ->
      List.iter (self # binding) sel
    | Type (pl, s, tm, doc) ->
      self # typedef tm ;
      self # comment doc
    | Method (abbrv, s, pl, body, ret, doc) ->
      self # abbrv abbrv ;
      List.iter (self # parameter) pl ;
      self # body body ;
      self # value ret ;
      self # comment doc
    | Function (s, pl, body, ret, doc) ->
      List.iter (self # parameter) pl ;
      self # body body ;
      self # value ret ;
      self # comment doc
    | Let (name, body, def, doc) ->
      self # body body ;
      self # value def ;
      self # comment doc
    | Exception (s, doc) ->
      self # comment doc
    | Inherits (n, abbrv1, abbrv2, doc) ->
      self # abbrv abbrv1 ;
      self # abbrv abbrv2 ;
      self # comment doc

  method variance (v : variance) = ()

  method typedef = function
    | Typedef (vis, v) -> self # value v
    | Gen_sym -> ()
    | Gen_id -> ()
    | Format -> ()

  method visibility (vis : visibility) = vis

  method value = function
    | Tuple v ->
      List.iter (self # value) v
    | Option (g, v) ->
      self # guard g ;
      self # value v
    | Value (l, s) ->
      self # mapping l ;
      self # storage s
    | Record fields ->
      List.iter
	(fun (s, v, doc) ->
	   self # value v ;
	   self # comment doc)
	fields
    | Variant cases ->
      List.iter
	(fun (s, g, v, doc) ->
	   self # guard g ;
	   List.iter (self # value) v ;
	   self # comment doc)
	cases
    | Tags (ns, v) -> ()

  method body = function 
    | Call_method (s, n, cs) ->
      self # storage s ;
      self # call_site cs
    | Call (s, cs) ->
      self # storage s ;
      self # call_site cs
    | Try (b, l) ->
      self # body b ;
      List.iter (fun (s, c) -> self # guard s ; self # const c) l
    | New (s, cs) ->
      self # storage s ;
      self # call_site cs
    | Access s ->
      self # storage s
    | Set (s, s') ->
      self # storage s ;
      self # storage s'
    | Inject (n, v) ->
      self # value v
    | Abs (s, b1, b2) ->
      self # body b1 ;
      self # body b2
    | Test (g, b1, b2) ->
      self # guard g ;
      self # body b1 ;
      self # body b2
    | Nop -> ()

  method guard = function
    | Const (s, c) -> self # storage s ; self # const c
    | Equals (s, s') -> self # storage s ; self # storage s'
    | Not g -> self # guard g
    | And (g1, g2) -> self # guard g1 ; self # guard g2
    | Or (g1, g2) -> self # guard g1 ; self # guard g2
    | Raise _ | True | False -> ()

  method call_site (s : call_site) = ()

  method abbrv (a : abbrv) = ()

  method storage = function
    | Global s -> ()
    | Var s -> ()
    | Arg (cs, i) -> self # call_site cs
    | Rest c -> self # call_site c
    | Unroll c -> self # call_site c
    | Field (s, n) -> self # storage s
    | Volatile c -> self #const c

  method mapping = function
    | Int | String | Bool | Float | Any | Void -> ()
    | Abbrv (abbrv, converters) ->
      self # abbrv abbrv
    | Array v -> self # value v
    | List v -> self # value v
    | Assoc v -> self # value v
    | Param string -> ()
    | Callback (pl, v) ->
      List.iter (self # parameter) pl ;
      self # value v
    | Handler (pl, v, Canceller b) ->
      List.iter (self # parameter) pl ;
      self # value v ;
      self # body b
    | Handler (pl, v, c) ->
      List.iter (self # parameter) pl ;
      self # value v


  method const (c : const) = ()

  method parameter (pt, n, doc, v) =
    self # comment doc ;
    self # value v

  method converters = function
    | Default -> ()
    | Custom v -> self # value v
    | Extern (p1, p2) -> ()
end

(** Stateful generic imperative iteration over an AST. An {!iter}
    equipped with predefined methods to set and acess the state. *)
class ['a] collect init = object
  inherit iter
  val mutable state : 'a = init
  method store v = state <- v
  method current = state
  method result = state
end
