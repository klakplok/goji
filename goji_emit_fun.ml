open Goji_types
open Printf
module StringMap = Map.Make (String)

(* common function between interface and implementation ***********************)

type env_item = {
  mutable modules : env_item StringMap.t ;
  mutable types : (string list * goji_type_definition) StringMap.t ;
}

let env = ref []

let enter_env () =
  env := { modules = StringMap.empty ; types = StringMap.empty } :: !env

let close_env name =
  match !env with
  | [] -> assert false
  | cur :: [] -> env := []
  | cur :: prev :: tl ->
    env := prev :: tl ;
    prev.modules <- StringMap.add name cur prev.modules

let register_type_in_env n params def =
  let cur = List.hd !env in
  cur.types <- StringMap.add n (params, def) cur.types

let rec find_type_in_env path n =
  match path with
  | [] ->
    let rec find env =
      match env with
      | [] -> raise Not_found
      | cur :: tl ->
	try StringMap.find n cur.types
	with Not_found -> find tl
    in find !env
  | mn :: mns ->
    let rec find env =
      match env with
      | [] -> raise Not_found
      | cur :: tl ->
	try
	  let m = StringMap.find mn cur.modules in
	  let rec find_m path m =
	    match path with
	    | [] -> StringMap.find n m.types
	    | mn :: mns -> find_m mns (StringMap.find mn m.modules)
	  in
	  try find_m mns m with Not_found -> raise Exit
	with
	| Not_found -> find tl
	| Exit -> raise Not_found
    in find !env

let reorder_args l =
  let rec reorder oacc acc l =
    match l with
    | [] -> List.rev oacc @ List.rev acc, (acc = [])
    | Optional _ as a :: tl -> reorder (a :: oacc) acc tl
    | a :: tl -> reorder oacc (a :: acc) tl
  in
  reorder [] [] l

let emit_indent fp indent =
  output_string fp (String.make indent ' ')

let emit_comment ?(inline = false) indent fp comment =
  let split_lines ?(width = 80) text =
    let text =
      let res = Buffer.create 100 in
      for i = 0 to String.length text - 1 do
	if not (text.[i] = '\n'
		&& (i = String.length text - 1
		    || text.[i + 1] <> '\n')) then
	  Buffer.add_char res text.[i]
      done ; Buffer.contents res
    in
    let splice s e = String.sub text s (e - s) in
    let max = String.length text in
    let rec split lsp lcut i res =
      if i >= max then
	List.rev (splice lcut i :: res)
      else
      if i - lcut >= width then
	if lsp > lcut then
	  split (succ lsp) (succ lsp) (succ lsp) (splice lcut lsp :: res)
	else
	  split lsp lcut (succ i) res
      else
	match text.[i] with
	| ' ' | '\t' -> split i lcut (succ i) res
	| '\n' -> split i (succ i) (succ i) (splice lcut i :: res)
	| _ -> split lsp lcut (succ i) res
    in split 0 0 0 []
  in
  match comment with
  | Nodoc -> ()
  | Doc text ->
    let lines = split_lines ~width:(80 - indent) text in
    match lines with
    | [] -> ()
    | [ l ] ->
      fprintf fp "%a(** %s *)%s"
	emit_indent indent l (if inline then "" else "\n")
    | l :: ls ->
      fprintf fp "%a(** %s" emit_indent indent l ;
      List.iter (fprintf fp "\n%a   %s" emit_indent (indent + 1)) ls ;
      fprintf fp " *)%s"  (if inline then "" else "\n")

let rec emit_type_def ?(converters = false) fp indent typs n com def =
  emit_comment indent fp com ;
  match def with
  | Abstract ->
    fprintf fp "%atype %a%s\n"
      emit_indent indent
      emit_type_params typs
      n ;
    if converters then (
      fprintf fp "%alet inject_%s = Inject.identity\n"
	emit_indent indent
	n ;
      fprintf fp "%alet extract_%s = Extract.identity\n\n"
	emit_indent indent
	n
    )
  | Extern _ ->
    fprintf fp "%atype %a%s%s\n\n"
      emit_indent indent
      emit_type_params typs
      n (if converters then " = any" else "") ;
    if converters then (
      fprintf fp "%alet inject_%s = Inject.identity\n"
	emit_indent indent
	n ;
      fprintf fp "%alet extract_%s = Extract.identity\n\n"
	emit_indent indent
	n
    )
  | Gen_sym ->
    fprintf fp "%atype %a%s%s\n\n"
      emit_indent indent
      emit_type_params typs
      n (if converters then " = string" else "") ;
    if converters then (
      fprintf fp "%alet inject_%s = Inject.string\n"
	emit_indent indent
	n ;
      fprintf fp "%alet extract_%s = Extract.string\n\n"
	emit_indent indent
	n ;
      fprintf fp "%alet current_%s = ref 0\n"
	emit_indent indent
	n ;
      fprintf fp "%alet make_%s () = incr current_%s ;"
	emit_indent indent
	n n ;
      fprintf fp " %S ^ string_of_int !current_%s\n\n"
	("goji_" ^ n ^ "_") n
    ) else (
      emit_comment indent fp (Doc (sprintf "Generates a fresh [%s]" n)) ;
      fprintf fp "%aval make_%s : unit -> %a%s\n\n"
	emit_indent indent
	n
	emit_type_params typs
	n
    )
  | Intern (Record ls) ->
    fprintf fp "%atype %a%s = {\n"
      emit_indent indent
      emit_type_params typs
      n ;
    List.iter
      (fun (n, ty, com) ->
	 fprintf fp "%a%s: %a %a ;\n"
	   emit_indent (indent + 2)
	   n
	   emit_type ty
	   (emit_comment ~inline:true 0) com)
      ls ;
    fprintf fp "%a}\n"
      emit_indent indent ;
    if converters then (
      fprintf fp "%alet inject_%s arg =\n"
	emit_indent indent
	n ;
      fprintf fp "%alet res = ref (Ops.constant \"undefined\") in\n"
	emit_indent (indent + 2) ;
      List.iter
	(fun (n, ty, com) ->
	  let njss = ref (0, StringMap.empty) in
	  emit_impl_matcher (indent + 2) fp (njss, "arg." ^ n, ty) ;
	  emit_impl_jss_injections (indent + 2) fp njss)
	ls ;
      fprintf fp "%a!res\n" emit_indent (indent + 2) ;
      fprintf fp "%alet extract_%s res =\n"
	emit_indent indent
	n ;
      fprintf fp "%a{\n" emit_indent indent ;
      List.iter
	(fun (n, ty, com) ->
	  fprintf fp "%a%s = %a ;\n"
	    emit_indent (indent + 2) n emit_impl_builder ty)
	ls ;
      fprintf fp "%a}\n\n" emit_indent indent ;
    )

  | Intern (Enum ls) ->
    fprintf fp "%atype %a%s =\n"
      emit_indent indent
      emit_type_params typs
      n ;
    List.iter
      (fun (n, guard, ty, com) ->
	match ty with
	| None ->
	  fprintf fp "%a| %s %a\n"
	    emit_indent (indent + 2)
	    (String.capitalize n)
	    (emit_comment ~inline:true 0) com
	| Some ty ->
	  fprintf fp "%a| %s of %a %a\n"
	    emit_indent (indent + 2)
	    (String.capitalize n)
	    emit_type ty
	    (emit_comment ~inline:true 0) com)
      ls ;
    if converters then (
      fprintf fp "%alet inject_%s arg = assert false\n"
	emit_indent indent
	n ;
      fprintf fp "%alet extract_%s arg = assert false\n"
	emit_indent indent
	n
    )
  | Intern desc ->
    fprintf fp "%atype %a%s = %a\n\n"
      emit_indent indent
      emit_type_params typs
      n
      emit_type desc ;
    if converters then (
      fprintf fp "%alet inject_%s arg =\n"
	emit_indent indent
	n ;
      fprintf fp "%alet res = ref (Ops.constant \"undefined\") in\n"
	emit_indent (indent + 2) ;
      let njss = ref (0, StringMap.empty) in
      emit_impl_matcher (indent + 2) fp (njss, "arg", desc) ;
      emit_impl_jss_injections (indent + 2) fp njss ;
      fprintf fp "%a!res\n" emit_indent (indent + 2) ;
      fprintf fp "%alet extract_%s res = %a\n\n"
	emit_indent indent
	n
	emit_impl_builder desc
    )
and emit_type_params fp typs =
  match typs with
  | [] -> ()
  | [p] ->
    fprintf fp "'%s " p
  | p :: ps  ->
    fprintf fp "('%s" p ;
    List.iter (fprintf fp ", '%s") ps ;
    fprintf fp ") "
and emit_type : type a. _ -> a goji_ocaml_value_desc -> unit = fun fp desc ->
  let rec emit_intf_value fp jsl =
    match jsl with
    | [] -> failwith "Unable to extract !"
    | (_, (Float_const _ | Int_const _ | String_const _ | Bool_const _ | Null_const | Undefined_const)) :: tl ->
      emit_intf_value fp tl
    | (_, Float) :: _ -> fprintf fp "float"
    | (acc, Nullable js) :: _ ->
      fprintf fp "%a option"
	emit_intf_value [acc, js]
    | (_, Int) :: _ -> fprintf fp "int"
    | (_, String)  :: _ -> fprintf fp "string"
    | (_, Bool) :: _ -> fprintf fp "bool"
    | (_, Param n) :: _ -> fprintf fp "'%s" n
    | (_, Abbrv (p, ms, n)) :: _ -> emit_type_alias fp (p, ms, n)
    | (_, Fun (args, ret)) :: _ ->
      fprintf fp "(%s%a%a)"
	(if args = [] then "unit -> " else "")
	emit_args_type args
	emit_return_type ret
    | (_, Array desc) :: _ ->  fprintf fp "%a array" emit_type desc
    | (_, Assoc desc) :: _ ->  fprintf fp "(string * %a) list" emit_type desc
  in
  match desc with
  | Tuple ([] | [ _ ]) -> assert false
  | Tuple ((e, _) :: es) ->
    fprintf fp "(" ;
    emit_type fp e ;
    List.iter (fun (e, _) -> fprintf fp " * %a" emit_type e) es ;
    fprintf fp ")"
  | Record ls -> failwith "inline record"
  | Enum ls -> failwith "inline enum"
  | Value jsl -> emit_intf_value fp jsl
  | Option (ty, _) -> fprintf fp "%a option" emit_type ty
and emit_type_alias fp (params, path, name) =
  let do_params = function
    | [] -> ()
    | [p] ->
      emit_type fp p ;
      fprintf fp " "
    | p :: ps  ->
      fprintf fp "(" ;
      emit_type fp p ;
      List.iter (fprintf fp ", %a" emit_type)	ps ;
      fprintf fp ") "
  in
  do_params params ;
  emit_path fp path ;
  fprintf fp "%s" name
and emit_path fp path =
  List.iter (fun m -> fprintf fp "%s." (String.capitalize m)) path
and emit_return_type fp ret =
  match ret with
  | None -> fprintf fp "unit"
  | Some ret -> emit_type fp ret
and emit_return indent fp ret =
  match ret with
  | None ->
    emit_indent fp indent ;
    fprintf fp "unit"
  | Some (ret, com) ->
    emit_indent fp indent ;
    emit_type fp ret
and emit_args_type fp args =
  List.iter
    (function
      | Curry (n, desc, com) ->
	fprintf fp "%a ->" emit_type desc
      | Optional (n, desc, com) ->
	fprintf fp "?%s: %a ->"n emit_type desc
      | Labeled (n, desc, com) ->
	fprintf fp "%s: %a ->"n emit_type desc
      | Phantom _ -> ())
    args
and emit_args indent fp args =
  List.iter
    (function
      | Curry (n, desc, com) ->
	fprintf fp "%a%a ->\n" emit_indent indent emit_type desc
      | Optional (n, desc, com) ->
	fprintf fp "%a?%s: %a ->\n" emit_indent indent n emit_type desc
      | Labeled (n, desc, com) ->
	fprintf fp "%a%s: %a ->\n" emit_indent indent n emit_type desc
      | Phantom _ -> ())
    args


(* implementation generation **************************************************)

  and emit_impl_module indent fp (name, comment, elements) =
    fprintf fp "%amodule %s = struct\n" emit_indent indent name ;
    enter_env () ;
    List.iter (emit_impl_module_element (indent + 2) fp) elements ;
    close_env name ;
    fprintf fp "%aend\n" emit_indent indent 
  and emit_impl_args indent fp args =
    List.iter
      (function
	| Curry (n, desc, com) ->
	  fprintf fp " %s" n
	| Optional (n, desc, com) ->
	  fprintf fp " ?%s" n
	| Labeled (n, desc, com) ->
	  fprintf fp " ~%s" n
	| Phantom _ -> ())
      args
  and emit_impl_matcher
      : type a. int -> _ -> (int * a goji_js_data_desc list StringMap.t) ref * _ * a goji_ocaml_value_desc -> unit
      = fun indent fp (jss, n, desc) ->
    let rec do_desc fp desc =
      match desc with
      | Tuple ([] | [ _ ]) -> assert false
      | Tuple ((e, _) :: es) ->
	fprintf fp "(" ;
	do_desc fp e ;
	List.iter (fun (e, _) -> fprintf fp ", " ; do_desc fp e) es ;
	fprintf fp ")"
      | Record ls -> failwith "inline record"
      | Enum ls -> failwith "inline enum"
      | Value js ->
	let nb = fst !jss + 1 in
	let vn = sprintf "v%x" nb in
	jss := (nb, StringMap.add vn js (snd !jss)) ;
	fprintf fp "%s" vn
    in
    match desc with
    | Value js ->
      jss := (fst !jss, StringMap.add n js (snd !jss))
    | _ ->
      fprintf fp "%alet %a = %s in\n" emit_indent indent do_desc desc n
  and emit_impl_jss_injections
	: type a. int -> _ -> (int * a goji_js_data_desc list StringMap.t) ref -> unit
	= fun indent fp jss ->
    StringMap.iter (fun n -> List.iter (fun (a, d) -> emit_impl_jss_injection indent fp (a, d, n))) (snd !jss)
  and emit_impl_jss_injection
	: type a. int -> _ -> a goji_js_data_accessor * goji_js_value_desc * _ -> unit
	= fun indent fp (js_acc, js_desc, n) ->
    let rec emit_impl_accessor
	: type a. _ -> a goji_js_data_accessor -> unit
	= fun fp acc ->
      match acc with
      | Var n -> fprintf fp "Ops.set_global %S" n
      | Root -> fprintf fp "res := "
      | Arg i -> fprintf fp "set_arg args %d " i
      | Rest -> fprintf fp "push_arg args "
      | Field (v, f) ->
	fprintf fp "Ops.set (%a) \"%s\" "
	  emit_impl_accessor' v f
      | Index (v, i) ->
	fprintf fp "Ops.set (%a) \"%d\" "
	  emit_impl_accessor' v i
    and emit_impl_accessor'
      : type a. _ -> a goji_js_data_accessor -> unit
      = fun fp acc ->
      match acc with
      | Var n -> fprintf fp "ensure_block_var %S" n
      | Root -> fprintf fp "ensure_block_res res"
      | Arg i -> fprintf fp "ensure_block_arg args %d" i
      | Field (v, f) ->
	fprintf fp "ensure_block_field (%a) %S"
	  emit_impl_accessor' v f
      | Rest -> failwith "bad use of variadic binder"
      | _ -> assert false
    in
    let inject fp (acc, js) =
      let rec inject fp js =
	match js with
	| Float_const f -> fprintf fp "Ops.of_float %g" f
	| Int_const i -> fprintf fp "Ops.of_int %d" i
	| String_const s -> fprintf fp "Ops.of_string %s" s
	| Bool_const b -> fprintf fp "Ops.of_bool %b" b
	| Null_const -> fprintf fp "Ops.constant \"null\""
	| Undefined_const -> fprintf fp "Ops.constant \"undefined\""
	| Float -> fprintf fp "Inject.float %s" n
	| Int -> fprintf fp "Inject.int %s" n
	| String -> fprintf fp "Inject.string %s" n
	| Bool -> fprintf fp "Inject.bool %s" n
	| Abbrv (_, [ "JavaScript" ], "any")
	| Param _ ->  fprintf fp "Inject.identity %s" n (* FIXME: check that *)
	| Abbrv (_, [ "JavaScript" ], nt) -> fprintf fp "Inject.%s %s" nt n
	| Abbrv (_, p, nt) -> fprintf fp "%ainject_%s %s" emit_path p nt n
	| Array desc ->
	  fprintf fp "Inject.array (fun arg ->\n" ;
	  fprintf fp "%alet res = ref (Ops.constant \"undefined\") in\n"
	    emit_indent (indent + 2) ;
	  let njss = ref (0, StringMap.empty) in
	  emit_impl_matcher (indent + 2) fp (njss, "arg", desc) ;
	  emit_impl_jss_injections (indent + 2) fp njss ;
	  fprintf fp "%a!res\n" emit_indent (indent + 2) ;
	  fprintf fp "%a) %s\n" emit_indent indent n
	| Assoc desc ->
	  fprintf fp "Inject.assoc (fun arg ->\n" ;
	  fprintf fp "%alet res = ref (Ops.constant \"undefined\") in\n"
	    emit_indent (indent + 2) ;
	  let njss = ref (0, StringMap.empty) in
	  emit_impl_matcher (indent + 2) fp (njss, "arg", desc) ;
	  emit_impl_jss_injections (indent + 2) fp njss ;
	  fprintf fp "%a!res\n" emit_indent (indent + 2) ;
	  fprintf fp "%a) %s\n" emit_indent indent n
	| Nullable js ->
	  fprintf fp "Inject.option (fun %s -> %a) %s"
	    n inject js n
	| Fun (args, ret) ->
	  let rec do_body = function
	    | Curry (n, desc, com) :: tl
	    | Labeled (n, desc, com) :: tl ->
	      fprintf fp "%alet %s = %a in\n"
		emit_indent (indent + 2) n emit_impl_builder desc ;
	      do_body tl
	    | Phantom _ :: _ ->
	      failwith "callbacks with phantom arguments not supported, sorry"
	    | Optional _ :: _ ->
	      failwith "callbacks with optional arguments not supported, sorry"
	    | [] ->
	      let indent = indent + 2 in
	      match ret with
	      | Some desc ->
		fprintf fp "%alet fraise = %s%a in \n"
		  emit_indent indent n do_call args ;
		fprintf fp "%alet res = ref (Ops.constant \"undefined\") in\n"
		  emit_indent indent ;
		let njss = ref (0, StringMap.empty) in
		emit_impl_matcher (indent + 2) fp (njss, "fraise", desc) ;
		emit_impl_jss_injections (indent + 2) fp njss ;
		fprintf fp "%a!res\n" emit_indent (indent + 2)
	      | None ->
		fprintf fp "%aignore (%s%a)\n" emit_indent indent n do_call args
	  and do_call fp = function
	    | [] -> if args = [] then fprintf fp " ()"
	    | Curry (n, desc, com) :: tl -> fprintf fp " %s%a" n do_call tl
	    | Labeled (n, desc, com) :: tl -> fprintf fp " ~%s%a" n do_call tl
	    | Phantom _ :: _ | Optional _ :: _ -> assert false
	  in
	  fprintf fp "Ops.wrap_fun (fun " ;
	  for i = 0 to max 0 (max_arg args - 1) do
	    fprintf fp "arg_%d " i
	  done ;
	  fprintf fp "-> \n" ;
	  do_body args ;
	  fprintf fp "%a)" emit_indent indent
      in fprintf fp "%a (%a)" emit_impl_accessor acc inject js
    in
    emit_indent fp indent ;
    inject fp (js_acc, js_desc) ;
    fprintf fp " ;\n"
  and emit_impl_arg_matcher indent fp (jss, arg) =
    match arg with
    | Curry (n, desc, com) ->
      emit_impl_matcher indent fp (jss, n, desc)
    | Optional (n, desc, com) ->
      fprintf fp "%a(match %s with\n" emit_indent indent n ;
      fprintf fp "%a | Some %s ->\n" emit_indent indent n ;
      let njss = ref (fst !jss + 1, StringMap.empty) in
      emit_impl_matcher (indent + 2) fp (njss, n, desc) ;
      emit_impl_jss_injections (indent + 2) fp njss ;
      fprintf fp "%a | None -> ()) ;\n" emit_indent indent
    | Labeled (n, desc, com) ->
      emit_impl_matcher indent fp (jss, n, desc)
    | Phantom _ -> ()
  and emit_impl_builder : type a. _ -> a goji_ocaml_value_desc -> unit = fun fp desc ->
    let rec extract fp jsl =
      match jsl with
      | [] -> failwith "Unable to extract !"
      | (acc, (Float_const _ | Int_const _ | String_const _ | Bool_const _ | Null_const | Undefined_const)) :: tl -> extract fp tl
      | (acc, Float) :: _ -> fprintf fp "Extract.float %a" emit_impl_accessor acc
      | (acc, Int) :: _ -> fprintf fp "Extract.int %a" emit_impl_accessor acc
      | (acc, String)  :: _ -> fprintf fp "Extract.string %a" emit_impl_accessor acc
      | (acc, Bool) :: _ -> fprintf fp "Extract.bool %a" emit_impl_accessor acc
      | (acc, Param _) :: _ 
      | (acc, Abbrv (_, [ "JavaScript" ], "any")) :: _ ->
	fprintf fp "Extract.identity %a" emit_impl_accessor acc
      | (acc, Abbrv (_, [ "JavaScript" ], nt)) :: _ -> fprintf fp "Extract.%s %a" nt emit_impl_accessor acc
      | (acc, Abbrv (_, p, nt)) :: _ -> fprintf fp "%aextract_%s %a" emit_path p nt emit_impl_accessor acc
      | (acc, Array desc) :: _ -> fprintf fp "Extract.array (fun res -> %a) %a" emit_impl_builder desc emit_impl_accessor acc
      | (acc, Nullable js) :: _ -> fprintf fp "Extract.option (fun res -> %a) %a" extract [acc, js] emit_impl_accessor acc
      | (acc, Fun (args, ret)) :: _ ->  failwith "functional extraction not yet available, sorry"
      | (acc, Assoc desc) :: _ -> failwith "assoc extraction not yet available, sorry"
    in
    let rec do_desc fp desc =
      match desc with
      | Tuple ([] | [ _ ]) -> assert false
      | Tuple ((e, _) :: es) ->
	fprintf fp "(" ;
        do_desc fp e ;
        List.iter (fun (e, _) -> fprintf fp ", " ; do_desc fp e) es ;
        fprintf fp ")"
      | Record ls -> failwith "inline record"
      | Enum ls -> failwith "inline enum"
      | Value jsl -> extract fp jsl
    in
    fprintf fp "%a" do_desc desc
  and emit_impl_res_builder fp res =
    match res with
    | Some (res, _) -> emit_impl_builder fp res
    | None -> fprintf fp "ignore res"
  and emit_impl_accessor : type a. _ -> a goji_js_data_accessor -> unit = fun fp acc ->
    match acc with
    | Var n -> fprintf fp "(Ops.symbol %S)" n
    | Root -> fprintf fp "res"
    | Arg i ->  fprintf fp "arg_%d" i (* FIXME: hack *)
    | Rest -> failwith "bad use of variadic binder"
    | Field (v, f) -> fprintf fp "(Ops.get %a \"%s\")" emit_impl_accessor v f
    | Index (v, i) -> fprintf fp "(Ops.get %a \"%d\")" emit_impl_accessor v i
  and emit_impl_module_element indent fp element =
    match element with
    | Function (n, com, acc, args, res) ->
      let args, add_unit = reorder_args args in
      let jss = ref (0, StringMap.empty) in
      fprintf fp "%alet %s\n%a%s =\n"
	emit_indent indent
	n
	(emit_impl_args indent) args
	(if add_unit then " () " else "");
      let indent = indent + 2 in
      fprintf fp "%alet fobj = %a in\n"
	emit_indent indent
	emit_impl_accessor acc ;
      fprintf fp "%alet args = alloc_args %d in\n"
	emit_indent indent (max_arg args) ;
      List.iter (fun arg -> emit_impl_arg_matcher indent fp (jss, arg)) args ;
      emit_impl_jss_injections indent fp jss ;
      fprintf fp "%alet res = Ops.call fobj (build_args args) in\n"
	emit_indent indent ;
      fprintf fp "%a%a\n\n"
	emit_indent indent
	emit_impl_res_builder res
    | Constructor ((_, tp, tn), n, com, args) ->
      let rec find tp tn =
	try
	  match find_type_in_env tp tn with
	  | (_, Extern acc) -> acc
	  | (_, Intern (Value [Root, Abbrv (_, tp, tn)])) -> find tp tn
	  | _ -> raise Not_found
	with Not_found ->
	  failwith (sprintf "cannot build a constructor for %s (not linked to a concrete extern type)" tn)
      in
      let acc = find tp tn in
      let args, add_unit = reorder_args args in
      let jss = ref (0, StringMap.empty) in
      fprintf fp "%alet %s\n%a%s =\n"
	emit_indent indent
	n
	(emit_impl_args indent) args
	(if add_unit then " () " else "") ;
      let indent = indent + 2 in
      fprintf fp "%alet cobj = %a in\n"
	emit_indent indent
	emit_impl_accessor acc ;
      fprintf fp "%alet args = alloc_args %d in\n"
	emit_indent indent (max_arg args) ;
      List.iter (fun arg -> emit_impl_arg_matcher indent fp (jss, arg)) args ;
      emit_impl_jss_injections indent fp jss ;
      fprintf fp "%alet res = Ops.call_constructor cobj (build_args args) in\n"
	emit_indent indent ;
      fprintf fp "%ares\n\n"
	emit_indent indent
    | Method (ty, n, com, args, res) ->
      let args, add_unit = reorder_args args in
      let jss = ref (0, StringMap.empty) in
      fprintf fp "%alet %s obj %a%a%a=\n"
	emit_indent indent
        n.ocaml_name
	(emit_impl_args indent) args
	(fun fp () -> if add_unit && args <> []  then fprintf fp "%a ()\n" emit_indent indent else ()) ()
        emit_indent indent ;
      let indent = indent + 2 in
      fprintf fp "%alet args = alloc_args %d in\n"
	emit_indent indent (max_arg args) ;
      List.iter (fun arg -> emit_impl_arg_matcher indent fp (jss, arg)) args ;
      emit_impl_jss_injections indent fp jss ;
      fprintf fp "%alet res = Ops.call_method obj %S (build_args args) in\n"
	emit_indent indent 
	n.js_name ;
      fprintf fp "%a%a\n\n"
	emit_indent indent
	emit_impl_res_builder res
    | Attribute (ty, n, com, desc, ro) ->
      (* getter *)
      fprintf fp "%alet %s obj =\n"
	emit_indent indent
	n.ocaml_name ;
      fprintf fp "%alet res = Ops.get obj %S in\n"
	emit_indent (indent + 2)
	n.js_name ;
      fprintf fp "%a%a\n\n"
	emit_indent (indent + 2)
	emit_impl_builder desc ;
      (* setter *)
      if not ro then (
        fprintf fp "%alet set_%s obj root =\n"
          emit_indent indent
	  n.ocaml_name ;
	fprintf fp "%alet res = ref (Ops.constant \"undefined\") in\n"
	  emit_indent (indent + 2) ;
	let njss = ref (0, StringMap.empty) in
	emit_impl_matcher (indent + 2) fp (njss, "root", desc) ;
	emit_impl_jss_injections (indent + 2) fp njss ; 
	fprintf fp "%aOps.set obj %S !res \n\n"
	  emit_indent (indent + 2) n.js_name ;
      )
    | Global (n, com, desc, ro) ->
      (* getter *)
      fprintf fp "%alet %s () =\n"
	emit_indent indent
	n ;
      fprintf fp "%a%a\n\n"
	emit_indent (indent + 2)
	emit_impl_builder desc ;
      (* setter *)
      if not ro then (
        fprintf fp "%alet set_%s v =\n"
          emit_indent indent
	  n ;
      (*    let njss = ref (0, StringMap.empty) in
	    emit_impl_matcher (indent + 2) fp (njss, "v", desc) ;
	    emit_impl_jss_injections (indent + 2) fp njss ; *)
	fprintf fp "%a()\n\n"
          emit_indent (indent + 2)
      )
    | Inherit (n, son, dad, com) ->
      fprintf fp "%alet %s x = Obj.magic x\n\n"
	emit_indent indent
	n
    | Type (typs, n, com, def) ->
      register_type_in_env n typs def ;
      emit_type_def ~converters:true fp indent typs n com def
    | Module m ->
      emit_impl_module indent fp m
    | Section _ -> ()
and emit_impl_main fp (name, comment, elements) =
  List.iter (fprintf fp "%s\n") Goji_emit_fun_static.impl_contents ;
  fprintf fp "\n" ;
  enter_env () ;
  List.iter (emit_impl_module_element 0 fp) elements ;
  close_env name

(* interface generation *******************************************************)

let rec emit_intf_main fp (name, comment, elements) =
  let rec emit_intf_module indent fp (name, comment, elements) =
    emit_comment indent fp comment ;
    fprintf fp "%amodule %s : sig\n" emit_indent indent name ;
    List.iter (emit_intf_module_element (indent + 2) fp) elements ;
    fprintf fp "%aend\n\n" emit_indent indent 
  and collect_intf_args_com args =
    List.fold_left
      (fun r a -> match a with
	 | Curry (n, _, Doc com) | Optional (n, _, Doc com)
	 | Labeled (n, _, Doc com) ->
	   r ^ sprintf "\n@param %s %s\n" n com
	 | _ -> r)
      "" args
  and collect_intf_return_com ret =
    match ret with
    | None | Some (_, Nodoc) -> ""
    | Some (_, Doc com) -> sprintf "\n@return: %s" com
  and emit_intf_module_element indent fp element =
    match element with
    | Function (n, com, acc, args, res) ->
      let args, add_unit = reorder_args args in
      let com = Doc ((match com with Nodoc -> "" | Doc s -> s)
		     ^ "\n" ^ collect_intf_args_com args
		     ^ collect_intf_return_com res) in
      emit_comment indent fp com ;
      fprintf fp "%aval %s :\n%a%s%a\n\n"
	emit_indent indent
	n
	(emit_args (indent + 2)) args
	(if add_unit then " unit ->" else "")
	(emit_return (indent + 2)) res
    | Constructor (ty, n, com, args) ->
      let args, add_unit = reorder_args args in
      let com = Doc ((match com with Nodoc -> "" | Doc s -> s)
		     ^ "\n" ^ collect_intf_args_com args) in
      emit_comment indent fp com ;
      fprintf fp "%aval %s : %a%s%a\n\n"
	emit_indent indent
	n
	(emit_args (indent + 2)) args
	(if add_unit then "unit -> " else "")
	emit_type (Value [Root, Abbrv ty])
    | Method (ty, n, com, args, res) ->
      let args, add_unit = reorder_args args in
      let com = Doc ((match com with Nodoc -> "" | Doc s -> s)
		     ^ "\n" ^ collect_intf_args_com args
		     ^ collect_intf_return_com res) in
      emit_comment indent fp com ;
      fprintf fp "%aval %s :\n%a%a ->\n%a%s%a\n\n"
	emit_indent indent
	n.ocaml_name
	emit_indent (indent + 2)
	emit_type (Value [Root, Abbrv ty])
	(emit_args (indent + 2)) args
	(if add_unit && args <> [] then "unit -> " else "")
	(emit_return (indent + 2)) res
    | Inherit (n, son, dad, com) ->
      emit_comment indent fp com ;
      fprintf fp "%aval %s :%a -> %a\n\n"
	emit_indent indent
	n
	emit_type (Value [Root, Abbrv son])
	emit_type (Value [Root, Abbrv dad])
    | Section (lvl, title) ->
      fprintf fp "%a(** {%d %s} *)\n\n"
	emit_indent indent
        lvl title
    | Attribute (ty, n, com, desc, ro) ->
      emit_comment indent fp com ;
      fprintf fp "%aval %s : %a -> %a\n\n"
	emit_indent indent
	n.ocaml_name
	emit_type (Value [Root, Abbrv ty])
	emit_type desc ;
      if not ro then (
	emit_comment indent fp (Doc "Setter function (see above).") ;
        fprintf fp "%aval set_%s : %a -> %a -> unit\n\n"
	  emit_indent indent
          n.ocaml_name
	  emit_type (Value [Root, Abbrv ty])
          emit_type desc
      )
    | Global (n, com, desc, ro) ->
      emit_comment indent fp com ;
      fprintf fp "%aval %s : unit -> %a\n\n"
	emit_indent indent
	n
	emit_type desc ;
      if not ro then (
	emit_comment indent fp (Doc "Setter function (see above).") ;
        fprintf fp "%aval set_%s : %a -> unit\n\n"
	  emit_indent indent
          n
          emit_type desc
      )
    | Type (typs, n, com, def) ->
      emit_type_def fp indent typs n com def ;
      let typs = List.map (fun n -> Value [Root, Param n]) typs in 
      fprintf fp "%a(**/**)\n" emit_indent indent ;
      fprintf fp "%aval inject_%s : %a -> JavaScript.any \n"
	emit_indent indent
	n
	emit_type (Value [Root, Abbrv (typs, [], n)]) ;
      fprintf fp "%aval extract_%s : JavaScript.any -> %a \n\n"
	emit_indent indent
	n 
	emit_type (Value [Root, Abbrv (typs, [], n)]) ;
      fprintf fp "%a(**/**)\n" emit_indent indent ;
      fprintf fp "\n\n"
    | Module m ->
      emit_intf_module indent fp m
  in
  emit_comment 0 fp comment ;
  fprintf fp "\n" ;
  List.iter (fprintf fp "%s\n") Goji_emit_fun_static.intf_contents ;
  fprintf fp "\n" ;
  List.iter (emit_intf_module_element 0 fp) elements
