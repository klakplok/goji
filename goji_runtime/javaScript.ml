(** Kernel of JavaScript types and operations *)

(** {2 Primitives on JavaScript Objects } *)

(** Generic JavaScript value *)
type any

(** Operations on JavaScript values *)
module Ops = struct
  external obj : (string * any) array -> any = "caml_js_object"
  external of_bool : bool -> any = "caml_js_from_bool"
  external to_bool : any -> bool = "caml_js_to_bool"
  external of_string : string -> any = "caml_js_from_string"
  external to_string : any -> string = "caml_js_to_string"
  external of_float : float -> any = "caml_js_from_float"
  external to_float : any -> float = "caml_js_to_float"
  external of_array : any array -> any = "caml_js_from_array"
  external to_array : any -> any array = "caml_js_to_array"
  external magic : 'a -> 'b = "%identity"
  external of_int : int -> any = "%identity"
  external to_int : any -> int = "%identity"
  external get_any : any -> any -> any = "caml_js_get"
  external set_any : any -> any -> any -> unit = "caml_js_set"
  let get : any -> string -> any = fun o f -> get_any o (of_string f)
  let set : any -> string -> any -> unit = fun o f v -> set_any o (of_string f) v
  external symbol : string -> any = "caml_js_var"
  let global : string -> any = fun n -> get (symbol "window") n
  let set_global : string -> any -> unit = fun n v -> set (symbol "window") n v
  external constant : string -> any = "caml_js_const"
  external call : any -> any array -> any = "caml_js_fun_call"
  external call_constructor : any -> any array -> any = "caml_js_new"
  external call_method : any -> string -> any array -> any = "caml_js_meth_call"
  let call_global : string -> any array -> any = fun s a -> call (symbol s) a
  external equals : any -> any -> bool = "caml_js_equals"
  external wrap_fun : ('a -> 'b) -> any = "caml_js_wrap_callback"
  let undefined = constant "undefined"
  let null = constant "null"
end

(** {2 Prodefined JavaScript Types } *)

(** The type of native, immutable JavaScript strings *)
type string = any

(**/**)
let inject_string (s : string) : any = s
let extract_string (s : any) : string = s
(**/**)

(** Operations on JavaScript strings *)
module String = struct
  type t = string

  (** Conversion to an OCaml string *)
  let to_string (ns : t) = Ops.to_string ns

  (** Conversion from an OCaml string *)
  let of_string s : t = Ops.of_string s

  (** Returns the length (number of UTF-16 code point) of a string *)
  let length (ns : t) =
    Ops.to_int (Ops.get ns "length")

  (** Access the nth character as its UTF-16 code point *)
  let get (ns : t) nth =
    if nth < 0 || nth > length ns then
      invalid_arg "JavaScript.String.get: index out of bounds" ;
    Ops.to_int (Ops.call_method ns "charCodeAt" [| Ops.of_int nth |])

  (** Calling [slice str start stop] returns the substring between
      character indexes start (included) and stop (excluded). A
      negative [index] is treated as [(length str) - index]. Out of
      bounds indexes are automatically changed to [0]/[length str]. *)
  let slice (ns : t) start stop : t =
    Ops.call_method ns "slice" [| Ops.of_int start ; Ops.of_int stop |]
end

(** The type of Date objects *)
type date = any

(**/**)
let inject_date (s : date) : any = s
let extract_date (s : any) : date = s
(**/**)

(** Operations on JavaScript Date objects *)
module Date = struct
  type t = date

  let date_of_milliseconds (ms : float) : t =
    Ops.call_constructor (Ops.symbol "Date") [| Ops.of_float ms |]

  let milliseconds_of_date (d : t) : float =
    Ops.to_float (Ops.call_method d "getTime" [||])
end

(** The type of DOM nodes *)
type node = any

(**/**)
let inject_node (s : node) : any = s
let extract_node (s : any) : node = s
(**/**)

(** Operations on DOM nodes *)
module Node = struct
  type t = node

  let document : t = Ops.symbol "document"
  let create tag : t = Ops.call_method document "createElement" [| Ops.of_string tag |]
  let create_text text : t = Ops.call_method document "createTextNode" [| Ops.of_string text |]
  let body () : t = Ops.get document "body"
  let get_element_by_id (n : t) id : t =
    let res = Ops.call_method n "getElementById" [| Ops.of_string id |] in
    if Ops.equals res (Ops.constant "undefined") then
      raise Not_found
    else
      res
  let append_child  (p : t) (n : t) =
    ignore (Ops.call_method p "appendChild" [| n |])
  let remove_child  (p : t) (n : t) =
    ignore (Ops.call_method p "removeChild" [| n |])
end

(** {2 Prodefined JavaScript Functions } *)

let console_debug msg =
  ignore (Ops.call_method (Ops.global "console") "debug" [| Ops.of_string msg |])

let console_debug_any obj =
  ignore (Ops.call_method (Ops.global "console") "debug" [| obj |])

(** {2 Conversions Between Worlds } *)

(** Conversions from OCaml to JavaScript *)
module Inject = struct
  let identity (i : 'a) : any = Ops.magic i
  let date (d : Date.t) : any = Ops.magic d
  let node (d : Node.t) : any = Ops.magic d
  let int (i : int) : any = Ops.magic i
  let unit (i : unit) : any = Ops.magic ()
  let float (i : float) : any = Ops.magic i
  let string i : any = Ops.of_string i
  let native_string (s : string) : any = Ops.magic s
  let bool (i : bool) : any = Ops.of_bool i
  let array (inject : 'a -> any) (a : 'a array) : any =
    Ops.magic (Ops.of_array (Array.map inject a))
  let assoc (inject : 'a -> any) (a : (_ * 'a) list) : any =
    let obj = Ops.obj [| |] in
    List.iter (fun (n, v) -> Ops.set obj n (inject v)) a ;
    obj
  let option (inject : 'a -> any) (v : 'a option) : any =
    match v with
    | None -> Ops.constant "undefined"
    | Some v -> inject v
  let fun_0
      (inject_ret : 'r -> any)
      (f : unit -> 'r) : any =
    Ops.wrap_fun
      (fun () -> inject_ret (f ()))
  let fun_1
      (extract_arg_1 : any -> 'arg_1)
      (inject_ret : 'r -> any)
      (f : 'arg_1 -> 'r) : any =
    Ops.wrap_fun (fun arg_1 ->
      inject_ret (f (extract_arg_1 arg_1)))
  let fun_2
      (extract_arg_1 : any -> 'arg_1)
      (extract_arg_2 : any -> 'arg_2)
      (inject_ret : 'r -> any)
      (f : 'arg_1 -> 'arg_2 -> 'r) : any =
    Ops.wrap_fun (fun arg_1 arg_2 ->
      inject_ret (f (extract_arg_1 arg_1)
		    (extract_arg_2 arg_2)))
end

(** Conversions from JavaScript to OCaml *)
module Extract = struct
  let identity (i : any) : 'a = Ops.magic i
  let date (d : any) : Date.t = Ops.magic d
  let node (d : any) : Node.t = Ops.magic d
  let int (i : any) : int = Ops.magic i
  let unit (i : any) : unit = Ops.magic ()
  let float (i : any) : float = Ops.magic i
  let string i = Ops.to_string i
  let native_string (s : any) : string = Ops.magic s
  let bool (i : any) : bool = Ops.to_bool i
  let array (extract : any -> 'a) (a : any) : 'a array =
    (Array.map extract (Ops.to_array (Ops.magic a)))
  let assoc (extract : any -> 'a) (a : any) : (_ * 'a) list =
    [ "undefined", Ops.constant "undefined" ]

  let option (extract : any -> 'a) (v : any) : 'a option =
    if Ops.equals v (Ops.constant "undefined") || Ops.equals v (Ops.constant "null") then
      None
    else
      Some (extract v)
  let fun_0
      (extract_ret : any -> 'r)
      (f : any) : unit -> 'r =
    (fun () ->
      extract_ret (Ops.call f [||]))
  let fun_1
      (inject_arg_1 : 'arg_1 -> any)
      (extract_ret : any -> 'r)
      (f : any) : 'arg_1 -> 'r =
    (fun arg_1 ->
      extract_ret (Ops.call f [| inject_arg_1 arg_1 |]))
  let fun_2
      (inject_arg_1 : 'arg_1 -> any)
      (inject_arg_2 : 'arg_2 -> any)
      (extract_ret : any -> 'r)
      (f : any) : 'arg_1 -> 'arg_1 -> 'r =
    (fun arg_1 arg_2 ->
      extract_ret (Ops.call f [| inject_arg_1 arg_1 ;
				 inject_arg_2 arg_2 |]))   
end
