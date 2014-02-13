(****************************************************************************)
(* GOJI (JavaScript Interface Generator for OCaml)                          *)
(* This file is published under the WTF Public Licence, which means that    *)
(* you can link it to your program, whatever its license, and even change   *)
(* its license for inclusion in your project if you need it for some reason.*)
(* (C) 2013 Benjamin Canou                                                  *)
(****************************************************************************)

(** Types and operations used by generated code *)

(* Kernel of JavaScript operations *******************************************)

type any

external js_obj : (string * any) array -> any = "caml_js_object"

external js_of_bool : bool -> any = "caml_js_from_bool"

external js_to_bool : any -> bool = "caml_js_to_bool"

external js_of_string : string -> any = "caml_js_from_string"

external js_to_string : any -> string = "caml_js_to_string"

external js_of_float : float -> any = "caml_js_from_float"

external js_to_float : any -> float = "caml_js_to_float"

external js_of_array : any array -> any = "caml_js_from_array"

external js_to_array : any -> any array = "caml_js_to_array"

external js_magic : 'a -> 'b = "%identity"

external js_of_int : int -> any = "%identity"

external js_to_int : any -> int = "%identity"

external js_get_any : any -> any -> any = "caml_js_get"

external js_set_any : any -> any -> any -> unit = "caml_js_set"

let js_get : any -> string -> any = fun o f -> js_get_any o (js_of_string f)

let js_set : any -> string -> any -> unit = fun o f v -> js_set_any o (js_of_string f) v

external js_symbol : string -> any = "caml_js_var"

let js_global : string -> any = fun n -> js_get (js_symbol "window") n

let js_set_global : string -> any -> unit = fun n v -> js_set (js_symbol "window") n v

external js_constant : string -> any = "caml_js_const"

external js_call : any -> any array -> any = "caml_js_fun_call"

external js_call_constructor : any -> any array -> any = "caml_js_new"

external js_call_method : any -> string -> any array -> any = "caml_js_meth_call"

let js_call_global : string -> any array -> any = fun s a -> js_call (js_symbol s) a

external js_equals : any -> any -> bool = "caml_js_equals"

external js_wrap_fun : ('a -> 'b) -> any = "caml_js_wrap_callback"

let js_undefined = js_constant "undefined"

let js_null = js_constant "null"

let js_nan = js_constant "NaN"

external js_instanceof : any -> any -> bool = "caml_js_instanceof"

let js_is_nan o =
  js_call (js_symbol "isNaN") [| o |] |> js_to_bool

(* Conversions from OCaml to JavaScript *************************************)

let inject_identity (i : 'a) : any = js_magic i

let inject_int (i : int) : any = js_magic i

let inject_unit (i : unit) : any = js_magic ()

let inject_float (i : float) : any = js_magic i

let inject_string (i : string) : any = js_of_string i

let inject_bool (i : bool) : any = js_of_bool i

let inject_array (inject : 'a -> any) (a : 'a array) : any =
  js_magic (js_of_array (Array.map inject a))

let inject_assoc (inject : 'a -> any) (a : (_ * 'a) list) : any =
  let obj = js_obj [| |] in
  List.iter (fun (n, v) -> js_set obj n (inject v)) a ;
  obj

(* Conversions from JavaScript to OCaml *************************************)

let extract_identity (i : any) : 'a = js_magic i

let extract_int (i : any) : int = js_magic i

let extract_unit (i : any) : unit = js_magic ()

let extract_float (i : any) : float = js_magic i

let extract_string (i : any) : string = js_to_string i

let extract_native_string (s : any) : string = js_magic s

let extract_bool (i : any) : bool = js_to_bool i

let extract_array (extract : any -> 'a) (a : any) : 'a array =
  (Array.map extract (js_to_array (js_magic a)))

let extract_assoc (extract : any -> 'a) (a : any) : (_ * 'a) list =
  failwith "Goji_internal.extract_assoc not implemented"

(* High level converters *****************************************************)

let inject_nonempty_array_or def (inject : 'a -> any) (a : 'a array) : any =
  match a with
  | [||] -> def
  | a -> inject_array inject a

let inject_nonempty_list_or def (inject : 'a -> any) (a : 'a list) : any =
  match a with
  | [] -> def
  | a -> inject_array inject (Array.of_list a)

let extract_nonempty_array_or def (extract : any -> 'a) (a : any) : 'a array =
  if js_equals a def then [||] else extract_array extract a

let extract_nonempty_list_or def (extract : any -> 'a) (a : any) : 'a list =
  if js_equals a def then [] else Array.to_list (extract_array extract a)

let inject_nonempty_array_or_undefined i a = inject_nonempty_array_or js_undefined i a
let inject_nonempty_list_or_undefined i a = inject_nonempty_list_or js_undefined i a
let extract_nonempty_array_or_undefined i a = extract_nonempty_array_or js_undefined i a
let extract_nonempty_list_or_undefined i a = extract_nonempty_list_or js_undefined i a

let inject_nonempty_array_or_null i a = inject_nonempty_array_or js_null i a
let inject_nonempty_list_or_null i a = inject_nonempty_list_or js_null i a
let extract_nonempty_array_or_null i a = extract_nonempty_array_or js_null i a
let extract_nonempty_list_or_null i a = extract_nonempty_list_or js_null i a

(* Arguments *****************************************************************)

type arg_block = { args : any array ; mutable rest : any list ; }

let alloc_args nb =
  { args = Array.make nb (js_constant "null") ;
    rest = [] }

let push_arg args arg =
  args.rest <- arg :: args.rest

let unroll_arg args arg =
  let len = js_to_int (js_get arg "length") in
  for i = 0 to len - 1 do
    push_arg args (js_get_any arg (js_of_int i))
  done

let set_arg args idx arg =
  args.args.(idx) <- arg

let build_args args =
  if args.rest = [] then args.args
  else Array.concat [ args.args ; Array.of_list (List.rev args.rest) ]

(* Dynamic allocation of blocks during injections ****************************)

let ensure_block_global n =
  let v = js_global n in
  if js_equals (js_constant "undefined") v then
    let b = js_obj [| |] in
    js_set_global n b ; b
  else v

let ensure_block_var res =
  if js_equals (js_constant "undefined") res then
    js_obj [| |]
  else res

let ensure_block_arg args idx =
  let v = args.args.(idx) in
  if js_equals (js_constant "undefined") v then
    let b = js_obj [| |] in
    args.args.(idx) <- b ; b
  else v

let ensure_block_field o f =
  let v = js_get_any o f in
  if js_equals (js_constant "undefined") v then
    let b = js_obj [| |] in
    js_set_any o f b ; b
  else v
