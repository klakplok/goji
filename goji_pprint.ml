(****************************************************************************)
(* GOJI (JavaScript Interface Generator for OCaml)                          *)
(* This file is published under the CeCILL licence                          *)
(* (C) 2013 Benjamin Canou                                                  *)
(****************************************************************************)

include PPrint

let int n =
  string (string_of_int n)

let format_tokens toks =
  group (separate_map (break 1) string toks)

let format_words text =
  group (separate (break 1) (words text))

let anest n doc =
  nesting (fun lvl -> nest (n - lvl) doc)

let align doc =
  nesting (fun lvl -> column (fun x -> nest (x - lvl) doc))

let (!^!) s = !^(Printf.sprintf "%S" s)

let (!^^) = format_words 

let (^^^) l r = l ^^ break 1 ^^ r

let (^^^?) l r =
  if l = empty || r = empty
  then l ^^ r
  else l ^^ break 1 ^^ r

let format_comment break_after text =
  if text = empty then
    empty
  else
    group (!^"(** " ^^ align (text ^^ !^ " *)"))
    ^^ if break_after then hardline else empty

let format_hidden text =
  if text = empty then
    empty
  else
    !^"(**/**)" ^^ hardline ^^ text ^^ hardline ^^ !^"(**/**)"

let format_module name ?(doc = empty) contents =
  format_comment true doc
  ^^ group
      (surround 2 1
         (format_tokens [ "module" ; String.capitalize name ; "=" ; "struct" ])
         contents
         (string "end"))
  ^^ hardline

let format_module_sig name ?(doc = empty) contents =
  format_comment true doc
  ^^ group
      (surround 2 1
         (format_tokens [ "module" ; String.capitalize name ; ":" ; "sig" ])
         contents
         (string "end"))
  ^^ hardline

let format_let_in pat value body =
  group
    (group
       (!^"let " ^^ group (align (pat ^^ break 1 ^^ !^"="))
        ^^ group (nest 2 (break 1 ^^ value)) ^^ break 1
        ^^ !^"in")
     ^^ (if body <> empty then break 1 ^^ body else empty))

let format_if cond bt bf =
  group
    (group
       (!^"if" ^^ group (nest 2 (break 1 ^^ cond) ^^^ !^"then ("))
     ^^ group (nest 2 (break 1 ^^ bt)) ^^ break 1 ^^ !^") "
     ^^ group (!^"else ("
               ^^ group (nest 2 (break 1 ^^ bf)) ^^ break 1 ^^ !^")"))

let format_match arg alts =
  group
    (group (!^"begin match" ^^ group (nest 2 (break 1 ^^ arg) ^^^ !^"with"))
     ^^ nest 2
         (break 1
          ^^ separate_map (break 1)
              (fun (pat, v) ->
		group (!^"| " ^^ pat
                       ^^ if v = empty then !^" -> ()"
			 else nest 2 (!^" ->" ^^^ !^"(" ^^ v ^^ !^")")))
	      alts)
     ^^^ !^"end")

let format_let pat ?(doc = empty) value =
  format_comment true doc
  ^^ group (!^"let " ^^ nest 2 (group (pat ^^^ !^"=") ^^^ group value))

let format_val name ?(doc = empty) ty =
  format_comment true doc
  ^^ group (!^"val" ^^^ name ^^^ !^":")
  ^^ group (nest 2 (break 1 ^^ ty))

let format_annot v t =
  group
    (!^"(" ^^ group (align v ^^ break 1)
     ^^ !^": " ^^ group (align t ^^ !^")"))

let format_fun_pat name ?(annot = empty) args =
  let args =
    if annot = empty then flow (break 1) args
    else flow (break 1) args ^^^ group (!^":" ^^ break 1 ^^ align annot)
  in
  name ^^^ group (align args)

let format_tuple ?(wrap = true) items =
  group
    ((if wrap then !^"(" else empty)
     ^^ column (fun c -> nest c (separate (!^"," ^^ break 1) items))
     ^^ (if wrap then !^")" else empty))

let format_record fs =
  match fs with
  | [] -> assert false
  | (f, v) :: [] ->
    group (!^"{ " ^^ f ^^ !^" =" ^^ nest 2 (break 1 ^^ v) ^^ !^" }")
  | (f, v) :: tl ->
    group
      (group (!^"{ " ^^ f ^^ !^" =" ^^ nest 2 (break 1 ^^ v) ^^ !^" ;")
       ^^ nest 2
           (let rec lines = function
              | [] -> assert false
              | (f, v) :: [] ->
                break 1
                ^^ group (f ^^ !^" =" ^^ nest 2 (break 1 ^^ v) ^^ !^" }")
              | (f, v) :: tl ->
                break 1
                ^^ group (f ^^ !^" =" ^^ nest 2 (break 1 ^^ v) ^^ !^" ;")
                ^^ lines tl
            in lines tl))

let format_record_type fs =
  let d doc = if doc = empty then empty else break 1 ^^ doc in
  match fs with
  | [] -> assert false
  | (f, v, doc) :: [] ->
    group (!^"{ " ^^ f ^^ !^" :" ^^ nest 2 (break 1 ^^ v ^^ d doc) ^^ !^" }")
  | (f, v, doc) :: tl ->
    group
      (group (!^"{ " ^^ f ^^ !^" :" ^^ nest 2 (break 1 ^^ v ^^ !^" ;" ^^ d doc))
       ^^ nest 2
           (let rec lines = function
              | [] -> assert false
              | (f, v, doc) :: [] ->
                break 1
                ^^ group (f ^^ !^" :" ^^ nest 2 (break 1 ^^ v ^^ d doc) ^^ !^" }")
              | (f, v, doc) :: tl ->
                break 1
                ^^ group (f ^^ !^" :" ^^ nest 2 (break 1 ^^ v ^^ !^" ;" ^^ d doc))
                ^^ lines tl
            in lines tl))

let format_sum_type cs =
  let field c = break 1 ^^ group (!^"*" ^^ break 1 ^^ c) in
  let args vs doc =
    match vs with
    | [] -> nest 2 (break 1 ^^ doc)
    | c :: cs ->
      !^" of " ^^ nest 2 (align (group (c ^^ concat_map field cs))
                          ^^ break 1 ^^ doc)
  in
  match cs with
  | [] -> assert false
  | cases ->
    group
      (separate_map (break 1)
         (fun (n, vs, doc) -> group (!^"| " ^^ n ^^ args vs doc))
         cases)
           
let format_array ?(wrap = true) items =
  let items = Array.to_list items in
  if items = [] then !^"[||]"
  else 
    group
      ((if wrap then !^"[| " else empty)
       ^^ align (flow (!^" ;" ^^ break 1) items)
       ^^ (if wrap then !^" |]" else empty))

let format_tuple_type ?(wrap = true) items =
  group
    ((if wrap then !^"(" else empty)
     ^^ column (fun c -> nest c (separate (break 1 ^^ !^"*" ^^ break 1) items))
     ^^ (if wrap then !^")" else empty))

let format_exception name ?(doc = empty) args =
  format_comment true doc
  ^^ group
      (!^"exception" ^^ break 1 ^^ !^name
       ^^ if args = [] then
           empty
         else
           break 1 ^^ !^"of" ^^ break 1
           ^^ separate (break 1 ^^ !^"*" ^^ break 1) args)
      
let format_phrases items =
  let items = List.filter ((<>) empty) items in
  separate (ifflat (break 1) (twice hardline)) items

let string_of_ident (tpath, tname) =
  List.fold_right (fun s r -> s ^ "." ^ r) tpath tname

let format_ident ident =
  !^(string_of_ident ident)

let format_app f args =
  group (!^"(" ^^ align (f ^^ (nest 2 (break 1 ^^ flow (break 1) args)) ^^ !^")"))

let format_ass r v =
  group (align (r ^^ !^" :=" ^^ (nest 2 (break 1 ^^ v))))

type sequence_item =
| Let_in of document * document
| Instruction of document

let format_sequence seq =
  let rec loop = function
    | [] -> empty
    | [ Let_in (pat, body) ] -> group (format_let_in pat body empty)
    | [ Instruction i ] -> group i
    | Let_in (pat, body) :: tl -> group (format_let_in pat body empty) ^^^ loop tl
    | Instruction i :: tl when i = empty -> loop tl
    | Instruction i :: tl -> group (i ^^ !^" ;") ^^^ loop tl
  in
  loop seq

let seq_instruction i = [ Instruction i ]
let seq_instructions is = List.map (fun i -> Instruction i) is
let seq_let_in pat body = [ Let_in (pat,body) ]
