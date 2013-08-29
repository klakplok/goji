(** Kernel of JavaScript types and operations *)

(** {2 Primitives on JavaScript Objects } *)

(** Generic JavaScript value *)
type any = Goji_internal.any

(** Operations on JavaScript values *)
module Ops = Goji_internal

(** {2 Prodefined JavaScript Types } *)

(** The type of native, immutable JavaScript strings *)
type js_string = any

(**/**)
let inject_js_string (s : js_string) : any = s
let extract_js_string (s : any) : js_string = s
(**/**)

(** Operations on JavaScript strings *)
module Js_string = struct
  type t = js_string

  (** Conversion to an OCaml string *)
  let to_string (ns : t) = Ops.js_to_string ns

  (** Conversion from an OCaml string *)
  let of_string s : t = Ops.js_of_string s

  (** Returns the length (number of UTF-16 code point) of a string *)
  let length (ns : t) =
    Ops.js_to_int (Ops.js_get ns "length")

  (** Access the nth character as its UTF-16 code point *)
  let get (ns : t) nth =
    if nth < 0 || nth > length ns then
      invalid_arg "JavaScript.String.get: index out of bounds" ;
    Ops.js_to_int (Ops.js_call_method ns "charCodeAt" [| Ops.js_of_int nth |])

  (** Calling [slice str start stop] returns the substring between
      character indexes start (included) and stop (excluded). A
      negative [index] is treated as [(length str) - index]. Out of
      bounds indexes are automatically changed to [0]/[length str]. *)
  let slice (ns : t) start stop : t =
    Ops.js_call_method ns "slice" [| Ops.js_of_int start ; Ops.js_of_int stop |]
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
    Ops.js_call_constructor (Ops.js_symbol "Date") [| Ops.js_of_float ms |]

  let milliseconds_of_date (d : t) : float =
    Ops.js_to_float (Ops.js_call_method d "getTime" [||])
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

  let document : t =
    Ops.js_symbol "document"

  let create tag : t =
    Ops.js_call_method document "createElement" [| Ops.js_of_string tag |]

  let create_text text : t =
    Ops.js_call_method document "createTextNode" [| Ops.js_of_string text |]

  let body () : t = Ops.js_get document "body"

  let get_element_by_id (n : t) id : t =
    let res = Ops.js_call_method n "getElementById" [| Ops.js_of_string id |] in
    if Ops.js_equals res (Ops.js_constant "undefined") then
      raise Not_found
    else
      res

  let append_child  (p : t) (n : t) =
    ignore (Ops.js_call_method p "appendChild" [| n |])

  let remove_child  (p : t) (n : t) =
    ignore (Ops.js_call_method p "removeChild" [| n |])
end

(** {2 Prodefined JavaScript Functions } *)

let console_debug msg =
  ignore (Ops.js_call_method (Ops.js_global "console") "debug" [| Ops.js_of_string msg |])

let console_debug_any obj =
  ignore (Ops.js_call_method (Ops.js_global "console") "debug" [| obj |])
