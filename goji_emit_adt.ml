(****************************************************************************)
(* GOJI (JavaScript Interface Generator for OCaml)                          *)
(* This file is published under the CeCILL licence                          *)
(* (C) 2013 Benjamin Canou                                                  *)
(****************************************************************************)

(** Back-end for OCaml with methods projected to simple functions over
    Abstract Data Type *)

open Goji_pprint
open Goji_messages
open Goji_ast

(** Ad-hoc compilation environment to track the used of variables in
    order to emit warnings / errors, produce nicer code, and maybe
    other stuff in the future. *)
module Env : sig
  type t
  val empty : t
  val def_ocaml_var : ?used:bool ->  string -> t -> t * document
  val let_ocaml_var : ?used:bool ->  string -> document -> t -> t * document
  val use_ocaml_var : string -> t -> document
  val exists_ocaml_var : string -> t -> bool
  val def_goji_var : ?used:bool -> ?ro:bool -> ?block:bool -> string -> t -> t * document
  val let_goji_var : ?used:bool -> ?ro:bool -> ?block:bool -> string -> document -> t -> t * document
  val use_goji_var : string -> t -> document
  val exists_goji_var : string -> t -> bool
  val warn_unused : t -> unit
  val goji_vars_diff : t -> t -> string list
  val merge_vars : string list list -> string list
  val tuple_goji_vars : string list -> t -> document
  val is_block : string -> t -> bool
  val is_ro : string -> t -> bool
end = struct
  module SM = Map.Make (String)

  type var =
  | Def of document ref
  | Let of document * document ref * document ref

  type env_var = (var * int ref * int) SM.t
  type goji_flags = (bool * bool) SM.t

  type t = env_var * env_var * goji_flags

  let uid = ref 0
  let uid () = incr uid ; !uid

  let use_var t n vars =
    try
      let var, rnb, _ = SM.find n vars in
      incr rnb ;
      match !rnb, var with
      | 1, Def (rv) ->
	rv := !^n ;
	document_ref rv
      | _, Def (rv) ->
	document_ref rv
      | 2, Let (v, rlet, rv) ->
	rlet := format_let_in !^n v empty ;
	rv := !^n ;
	document_ref rv
      | _, Let (_, _, rv) ->
	document_ref rv
    with Not_found ->
      error "undefined %s variable %S" t n

  let exists_var n vars =
    try
      ignore (SM.find n vars) ; true
    with Not_found ->
      false

  let def_var t ?(used = false) n vars =
    let r = ref !^"_" in
    let nenv = SM.add n (Def r, ref 0, uid ()) vars in
    if used then ignore (use_var t n nenv) ;
    nenv, document_ref r

  let let_var t ?(used = false) n v vars =
    let rlet = ref empty and rvar = ref v and rnb = ref 0 in
    let ilet = document_ref rlet in
    let nenv = SM.add n (Let (v, rlet, rvar), rnb, uid ()) vars in
    if used then ignore (incr rnb ; use_var t n nenv) ;
    nenv, ilet

  let warn_unused (ovars, gvars, gflags) =
    SM.iter
      (fun v (_, nb, _) ->
	if !nb = 0 && v <> "()" then warning "unused OCaml variable %S" v)
      ovars ;
    SM.iter
      (fun v (_, nb, _) ->
	if !nb = 0 then warning "unused Goji variable %S" v)
      gvars

  let use_ocaml_var n (ovars, gvars, gflags) =
    use_var "OCaml" n ovars

  let exists_ocaml_var n (ovars, gvars, gflags) =
    exists_var n ovars
      
  let def_ocaml_var ?(used = false) n (ovars, gvars, gflags) =
    let ovars, res = def_var "OCaml" ~used n ovars in
    (ovars, gvars, gflags), res

  let let_ocaml_var ?(used = false) n v (ovars, gvars, gflags) =
    let ovars, res = let_var "OCaml" n v ovars in
    (ovars, gvars, gflags), res

  let use_goji_var n (ovars, gvars, gflags) =
    use_var "Goji" n gvars

  let exists_goji_var n (ovars, gvars, gflags) =
    exists_var n gvars

  let def_goji_var ?(used = false) ?(ro = true) ?(block = false) n (ovars, gvars, gflags) =
    (try if fst (SM.find n gflags) then
	error "trying to redefine read-only Goji variable %S" n
     with Not_found -> ());
    let gvars, res = def_var "Goji" ~used n gvars in
    let gflags = SM.add n (ro, block) gflags in
    (ovars, gvars, gflags), res

  let let_goji_var ?(used = false) ?(ro = true) ?(block = false) n v (ovars, gvars, gflags) =
    (try if fst (SM.find n gflags) then
	error "trying to redefine read-only Goji variable %S" n
     with Not_found -> ());
    let gvars, res = let_var "Goji" n v gvars in
    let gflags = SM.add n (ro, block) gflags in
    (ovars, gvars, gflags), res

  let is_block n (ovars, gvars, gflags) =
    snd (SM.find n gflags)

  let is_ro n (ovars, gvars, gflags) =
    fst (SM.find n gflags)

  let goji_vars_diff (_, gvars1, _) (_, gvars2, _) =
    SM.fold
      (fun v (_, _, id1) r ->
	try
	  let _, _, id2 = SM.find v gvars1 in
	  if id1 = id2 then r else v :: r
	with Not_found -> v :: r)
      gvars2 []

  let empty = (SM.empty, SM.empty, SM.empty)

  module SS = Set.Make (String)
    
  let merge_vars lists = 
    SS.elements (List.fold_right (List.fold_right SS.add) lists SS.empty)
      
  let tuple_goji_vars vars env =
    format_tuple
      (List.map
	 (fun v ->
	   if exists_goji_var v env then
	     use_goji_var v env
	   else
	     !^"G.js_constant \"undefined\"")
	 vars)
end

(** Code emission class *)
class emitter = object (self)
  inherit Goji_emit_struct.emitter as mommy

  (* Utility methods **********************************************************)

  method format_type_params tparams =
    let format_param = function
      | None, n -> !^("'" ^ n)
      | Some Covariant, n -> !^("+'" ^ n)
      | Some Contravariant, n -> !^("-'" ^ n)
    in
    match tparams with
    | [] -> empty
    | [ p ] -> format_param p ^^ !^" "
    | _ ->
      format_tuple (List.map format_param tparams)
      ^^ break 1

  method format_type_args targs =
    match targs with
    | [] -> empty
    | [ p ] -> self # format_value_type p ^^ !^" "
    | _ ->
      format_tuple (List.map (fun p -> self # format_value_type p) targs)
      ^^ break 1

  (** @param sa: put parentheses around functional types
      @param st: put parentheses around tuple types *)
  method format_value_type ?(sa = false) ?(st = false) = function
    | Tuple vs ->
      format_tuple_type ~wrap:st
	(List.map (self # format_value_type ~sa:true ~st:true) vs)
    | Record fields ->
      format_record_type
        (List.map
           (fun (n, def, doc) ->
              (!^n,  self # format_value_type def,
               format_comment false (self # format_doc doc)))
           fields)
    | Variant cases ->
      format_sum_type
        (List.map
           (fun (n, guard, defs, doc) ->
              (!^n,  List.map (self # format_value_type) defs,
               format_comment false (self # format_doc doc)))
           cases)
    | Value (Int, _) -> !^"int"
    | Value (String, _) -> !^"string"
    | Value (Bool, _) -> !^"bool"
    | Value (Float, _) -> !^"float"
    | Value (Any, _) -> !^"JavaScript.any"
    | Value (Void, _) -> !^"unit"
    | Option (_, v) ->
      self # format_value_type ~sa:true ~st:true v ^^ !^" option"
    | Value (Array v, _) ->
      self # format_value_type ~sa:true ~st:true v ^^ !^" array"
    | Value (List v, _) ->
      self # format_value_type ~sa:true ~st:true v ^^ !^" list"
    | Value (Assoc v, _) ->
      !^"(string * " ^^ self # format_value_type v ~sa:true ~st:true ^^ !^") list"
    | Value (Param n, _) -> !^("'" ^ n)
    | Value (Abbrv ((targs, tname), _), _) ->
      self # format_type_args targs ^^ format_ident tname
    | Value (Handler (params, ret, _), _)
    | Value (Callback (params, ret), _) ->
      (if sa then !^"(" else empty)
      ^^ align (self # format_fun_type params ret)
      ^^ (if sa then !^")" else empty)

  (** Constructs the OCaml arrow type from the defs of parameters and
      return value. Does not put surrounding parentheses. *)
  method format_fun_type params ret =
    let format_one (pt, name, doc, def) =
      let pref, def = match pt with
	| Optional -> !^"?" ^^ !^name ^^ !^":", def
	| Curry -> empty, def
	| Labeled -> !^name ^^ !^":", def
      in
      group (pref ^^ self # format_value_type ~sa:true def ^^^ !^"->")
    in
    let rec group_curry = function
      | (Curry, _, _, _) as a :: ((Curry, _, _, _) :: _ as tl) ->
	(match group_curry tl with
	 | [] -> assert false
	 | r :: tl -> (a :: r) :: tl)
      | a :: tl -> [a] :: group_curry tl
      | [] -> []
    in
    separate (break 1)
      (List.map
	 (fun l -> group (separate_map (break 1) format_one l))
	 (group_curry params)
       @ [ group (self # format_value_type ~sa:true ret) ])

  (** Construct the coment block for a function, with the provided
      doc, a call example and the list of parameters. *)
  method format_function_doc fdoc name params =
    let max =
      List.fold_left
	(fun r (_, name, _, _) -> max (String.length name) r)
	0 params + 2
    in
    let pad str =
      let res = String.make max ' ' in
      String.blit str 0 res 0 (String.length str) ;
      res
    in
    let doc, example =
      (List.fold_right
	 (fun param (rd, re) ->
	    let name, doc, ex =
	      match param with
	      | Curry, name, doc, _ -> name, doc, name
	      | _, name, doc, _ -> name, doc, "~" ^ name
	    in
	    let doc = self # format_doc doc in
	    if doc = empty then (rd, re)
	    else
              (* FIXME: @param does not work for curried args *)
	      ((group (!^("@param " ^ pad ( name )) ^^ break 1)
		^^ group (align doc)) :: rd, ex :: re))
	 params ([], []))
    in
    let doc = separate hardline doc in
    if doc = empty then
      self # format_doc fdoc
    else
      self # format_doc fdoc
      ^^ twice hardline
      ^^ !^"Example call:" ^^
	group (nest 2
		 (break 1 ^^ !^"[" ^^ !^name ^^ !^" "
		  ^^ align (flow (break 1) (List.map string (example)))
		  ^^ !^"]"))
      ^^ hardline
      ^^ doc

  (* Injection methods ********************************************************)

  method format_injector_definition name def =
    let env, var = Env.(def_ocaml_var "obj" empty) in
    let env, body = self # format_injector_body "obj" def env in
    let res = format_let (!^("inject_" ^ name) ^^^ var) body in
    Env.warn_unused env ; res

  method format_injector_body var def env =
    let def = Goji_dsl.(def @@ Var "res") in
    let env, code = self # format_injector var def env in
    let body = code @ seq_result (Env.use_goji_var "res" env) in
    env, format_sequence body

  method format_arguments_injection params env =
    let format_param (env, prev) (pt, name, _, def) =
      let def = match pt with
	| Optional -> Option (True, def)
        | Curry | Labeled -> def
      in
      let env = fst (Env.def_ocaml_var name env) in
      let env, seq = self # format_injector name def env in
      env, prev @ seq
    in
    List.fold_left format_param (env, []) params

  method format_injector ?(path = []) v def env =
    match def with
    | Record fields ->
      List.fold_left
        (fun (env, prev) (n, def, doc) ->
	  let var = "f'" ^ n in
	  let env, vlet =
	    let vn = Env.use_ocaml_var v env ^^ !^"." ^^ format_ident (path, n) in
	    Env.let_ocaml_var var vn env
	  in
	  let env, seq = self # format_injector var def env in
	  (env, prev @ seq_instruction' vlet @ seq))
	(env, [])
        fields
    | Variant cases ->
      let branches =
	List.map
          (fun (n, g, defs, doc) ->
	    let env, resg = self # format_guard_injector g env in
	    if defs = [] then
	      env, resg, !^n
	    else
	      let env, code, _, decls =
		List.fold_right
		  (fun def (env, code, i, tup) ->
		    let vn = v ^ "'" ^ string_of_int i in
		    let env, decl = Env.def_ocaml_var vn env in
		    let env, resd = self # format_injector vn def env in
		    (env, code @ resd, succ i, decl :: tup))
		  defs (env, resg, 0, [])
	       in env, resg @ code, !^n ^^ !^" " ^^ format_tuple decls)
	  cases
      in
      let nvars =
	Env.merge_vars
	  (List.map
	     (fun (env', _, _) -> Env.goji_vars_diff env env')
	     branches)
      in
      if nvars = [] then
	env,
	seq_instruction
	  (format_match (Env.use_ocaml_var v env)
	     (List.map
		(fun (_, code, pat) -> pat, format_sequence code)
		branches))
      else
	let body =
	  format_match (Env.use_ocaml_var v env)
	    (List.map
	       (fun (env, code, pat) ->
		 let reti = seq_result (Env.tuple_goji_vars nvars env) in
		 pat, format_sequence (code @ reti))
	       branches)
	in
	let env =
	  List.fold_left
	    (* this is a horrid hack, thank me very much if you have to read this *)
	    (fun env v -> fst (Env.let_goji_var ~used:true v !^v env))
	    env nvars
	in
	env, seq_let_in (format_tuple (List.map (!^) nvars)) body 
    | Tuple (defs) ->
      let env, _, decls, code =
	List.fold_left
	  (fun (env, i, decls, code) def ->
	    let var = v ^ "'" ^ string_of_int i in
	    let env, decl = Env.def_ocaml_var var env in
	    let env, instrs = self # format_injector var def env in
	    (env, succ i, decl :: decls, code @ instrs))
	  (env,0, [], [])
	  defs
      in
      let decls = List.rev decls in
      env, seq_let_in (format_tuple decls) (Env.use_ocaml_var v env) @ code
    | Option (g, d) ->
      let vn = v ^ "'v" in
      let envd = fst (Env.def_ocaml_var vn env) in
      let envd, resd = self # format_injector vn d envd in
      let envg, resg = self # format_guard_injector g env in
      let nvarsd = Env.goji_vars_diff env envd in
      let nvarsg = Env.goji_vars_diff env envg in
      let nvars = Env.merge_vars [ nvarsg ;  nvarsd ] in
      if nvars = [] then
	env,
	seq_instruction
	  (format_match (Env.use_ocaml_var v env)
             [ !^("Some " ^ vn), format_sequence resd ;
               !^"None", format_sequence resg ])
      else
	let body =
	  format_match (Env.use_ocaml_var v env)
            [ !^("Some " ^ vn),
	      format_sequence (resd @ seq_result (Env.tuple_goji_vars nvars envd)) ;
              !^"None",
	      format_sequence (resg @ seq_result (Env.tuple_goji_vars nvars envg)) ] in
	let env =
	  List.fold_left
	    (* this is a horrid hack, thank me very much if you have to read this *)
	    (fun env v -> fst (Env.let_goji_var ~used:true v !^v env))
	    env nvars
	in
	env, seq_let_in (format_tuple (List.map (!^) nvars)) body 	  
    | Value (Void, sto) -> env, []
    | Value (leaf, sto) ->
      let env, arg = self # format_leaf_injector v leaf env in
      self # format_storage_assignment arg sto env

  method format_guard_injector g env =
    let rec collect = function
      | Const (sto, c) -> [ (sto, c) ]
      | Raise _ | True | False | Not _ -> []
      | And (g1, g2) -> collect g1 @  collect g2
      | Or (g, _) -> collect g
    in
    let env, seq =
      List.fold_left
	(fun (env, seq) (sto, c) ->
	  let env, instrs =
	    self # format_storage_assignment
	      (self # format_const c)
	      sto env
	  in env, instrs @ seq)
	(env, [])
	(collect g)
    in
    env, seq

  method format_leaf_injector v leaf env =
    match leaf with
    (* simple types *)
    | Int -> env, format_app !^"G.inject_int" [ Env.use_ocaml_var v env ]
    | String -> env, format_app !^"G.inject_string" [ Env.use_ocaml_var v env ]
    | Bool -> env, format_app !^"G.inject_bool" [ Env.use_ocaml_var v env ]
    | Float -> env, format_app !^"G.inject_float" [ Env.use_ocaml_var v env ]
    | Any -> env, Env.use_ocaml_var v env
    | Void -> env, empty
    (* higher order injections *)
    | Array def ->
      let local, decl = Env.def_ocaml_var "item" env in
      env, format_app
        !^"G.inject_array"
        [ format_fun [ decl ] (snd (self # format_injector_body "item" def local)) ;
          Env.use_ocaml_var v env ]
    | List def ->
      let local, decl = Env.def_ocaml_var "item" env in
      env, format_app !^"Array.to_list" [
	format_app
          !^"G.inject_array"
          [ format_fun [ decl ] (snd (self # format_injector_body "item" def local)) ;
            Env.use_ocaml_var v env ] ]
    | Assoc def ->
      let local, decl = Env.def_ocaml_var "item" env in
      env, format_app
        !^"G.inject_assoc"
        [ format_fun [ decl ] (snd (self # format_injector_body "item" def local)) ;
          Env.use_ocaml_var v env ]
    (* named types *)
    | Param _ -> (* TODO: parametric injections *)
      env, format_app !^"G.inject_identity" [ Env.use_ocaml_var v env ]
    | Abbrv ((_, (path, name)), Default) ->
      env, format_app (format_ident (path, "inject_" ^ name)) [ Env.use_ocaml_var v env ]
    | Abbrv (abbrv, Extern (inject, extract)) ->
      env, format_app (format_ident inject) [ Env.use_ocaml_var v env ]
    | Abbrv (abbrv, Custom def) ->
      let local, decl = Env.def_ocaml_var "v" env in
      env, format_app
        (format_fun [ decl ] (snd (self # format_injector_body "v" def local)))
        [ Env.use_ocaml_var v env ]
    (* functional types *)
    | Callback (params, ret)
    | Handler (params, ret, _) ->
      (* Generates the following pattern:
	  Ops.wrap_fun
	   (fun args'0 ... args'n ->
	     let cbres = v (extract arg_1) ... (extract arg_n) in
	     inject cbres) *)      
      let max_arg =
	let collect = object (self)
 	  inherit [int] collect 0 as mom
	  method! storage = function
	    | Arg ("args", i) ->
	      self # store (max (self # current) (i + 1))
	    | Arg (_, _) | Rest _ -> failwith "error 8845"
	    | oth -> mom # storage oth
	end in
	List.iter (collect # parameter) params ;
	collect # result
      in
      let rec args i env =
	if i = 0 then
	  env, []
	else
	  let env, decl = Env.def_goji_var ("args'" ^ string_of_int (i - 1)) env in
	  let env, decls = args (i - 1) env in
	  env, decl :: decls
      in
      let local, args = args max_arg env in
      let format_param (pt, name, _, def) (env, args) =
	match pt with
	| Optional ->
	  error "unsupported optional argument in callback"
	| Curry ->
	  let env, arg = self # format_extractor def env in
	  env, arg :: args 
	| Labeled ->
	  let env, arg = self # format_extractor def env in
	  let arg = !^"~" ^^ !^name ^^ !^":" ^^ arg in
	  env, arg :: args
      in
      let local, params = List.fold_right format_param params (local, []) in
      (* do not inject unit results *)
      let fun_body =
	match ret with
	| Value (Void, _) ->
	  format_app (Env.use_ocaml_var v local) params
	| _ ->
	  let call = format_app (Env.use_ocaml_var v local) params in
	  let local, vlet = Env.let_ocaml_var "cbres" call local in
	  let _, body = self # format_injector_body "cbres" ret local in
	  (format_sequence
	     (seq_instruction' vlet
	      @ seq_result body))
      in
      env,
      format_app
	!^"G.js_wrap_fun"
	[ format_fun (if max_arg = 0 then [ !^" ()" ] else args) fun_body ]

  method format_storage_assignment arg sto env =
    let rec toplevel sto =
      match sto with
      | Global n ->
	let body = format_app ~wrap:false !^"G.js_set_global" [ !^!n ; arg ] in
	env, seq_instruction body
      | Var n ->
	let env, v = Env.let_goji_var n arg env in
	env, seq_instruction' v
      | Arg (cs, n) ->
	env,
	seq_instruction (format_app ~wrap:false !^"G.set_arg" [ !^(cs ^ "'A") ; int n ; arg ])
      | Rest cs ->
	env,
	seq_instruction (format_app ~wrap:false !^"G.push_arg" [ !^(cs ^ "'A") ; arg ])
      | Field (sto, n) ->
	let preq, env, blo = nested sto env in
	env,
	preq
	@ (seq_instruction (format_app ~wrap:false !^"G.js_set" [ blo ; !^!n ; arg ]))
      | Cell (sto, i) ->
	let preq, env, blo = nested sto env in
	env,
	preq 
        @ (seq_instruction
	     (format_app ~wrap:false
		!^"G.js_set_any "
		[ blo ;
		  !^"(G.js_of_int" ^^^ !^(string_of_int i) ^^ !^")" ;arg ]))
    and nested sto env =
      match sto with
      | Rest cs ->
	error "indirect assignment of rest not supported"
      | Global n ->
        [], env, format_app !^"G.ensure_block_global" [ !^!n ]
      | Var n ->
	let env, slet =
	  if Env.(not (exists_goji_var n env)) then
	    let env, rlet =
	      Env.let_goji_var ~block:true n !^"(G.js_obj [| |])" env
	    in env, seq_instruction' rlet
	  else if Env.(not (is_ro n env || is_block n env)) then
	    let env, rlet =
	      Env.let_goji_var ~block:true n
		(format_app
		   !^"G.ensure_block_var"
		   [ Env.use_goji_var n env ])
		env
	    in env, seq_instruction' rlet
	  else env, []
	in
	slet, env, Env.use_goji_var n env
      | Arg (cs, n) ->
        [], env, format_app !^"G.ensure_block_arg" [ !^(cs ^ "'A") ; int n ]
      | Field (sto, n) ->
	let preq, env, res = nested sto env in
        preq, env, format_app !^"G.ensure_block_field" [ res ; !^!n ]
      | Cell (sto, i) ->
	let preq, env, res = nested sto env in
        preq, env, format_app !^"G.ensure_block_cell" [ res ; int i ]
    in toplevel sto

  (* Extraction methods *******************************************************)

  method format_extractor_definition name def =
    let env, decl = Env.(def_goji_var "root" empty) in
    let env, body = self # format_extractor_body def env in
    Env.warn_unused env ;
    format_let (!^("extract_" ^ name) ^^^ decl) body

  method format_extractor_body def env =
    self # format_extractor def env

  method format_result_extractor def env =
    match def with
    | Value (Void, _) -> env, []
    | _ -> 
      let env, res = self # format_extractor def env in
      env, seq_result res

  (** produces code that extracts an OCaml value of structure [def]
      from the context *)
  method format_extractor def env =
    match def with
    | Record fields ->
      let env, fields =
        List.fold_right
          (fun (n, def, doc) (env, res) ->
	    let env, body = self # format_extractor def env in
	    env, (!^n, body) :: res)
          fields (env, [])
      in
      env, format_record fields
    | Variant cases ->
      List.fold_right
        (fun (n, g, defs, doc) (env, alt) ->
	  let env, args =
            if defs = [] then
	      env, !^n
	    else
	      let env, args =
		List.fold_right
		  (fun def (env, rs) ->
		    let env, r = self # format_extractor def env in
		    env, r :: rs)
		  defs
		  (env, [])
	      in
	      env, !^n ^^ (nest 2 (break 1 ^^ format_tuple args))
	  in
          env, format_if (self # format_guard g env) args alt)
        cases
        (env, !^("failwith \"unable to extract\"" (* FIXME: type name *)))
    | Tuple (defs) ->
      let env, comps =
	List.fold_right
	  (fun def (env, rs) ->
	    let env, r = self # format_extractor def env in
	    env, r :: rs)
	  defs
	  (env, [])
      in
      env, format_tuple comps
    | Option (g, d) ->
      let env, arg = self # format_extractor d env in
      env,
      format_if
	(self # format_guard g env)
        !^"None"
	(format_app !^"Some "[ arg ])
    | Value (Void, _) -> env, !^"()"
    | Value (leaf, sto) ->
      let arg = self # format_storage_access sto env in
      env, self # format_leaf_extractor leaf arg env

  method format_leaf_extractor leaf arg env =
    match leaf with
    | Int -> format_app !^"G.extract_int" [ arg ]
    | String -> format_app !^"G.extract_string" [ arg ]
    | Bool -> format_app !^"G.extract_bool" [ arg ]
    | Float -> format_app !^"G.extract_float" [ arg ]
    | Any -> arg
    | Void -> assert false
    | Array def ->
      let local, decl = Env.def_goji_var "root" env in
      format_app
        !^"G.extract_array"
        [ format_fun [ decl ] (snd (self # format_extractor def local)) ;
	  arg ]
    | List def ->
      let local, decl = Env.def_goji_var "root" env in
      format_app
	!^"Array.to_list"
	[ format_app
            !^"G.extract_array"
            [ format_fun [ decl ] (snd (self # format_extractor def local)) ;
	      arg ] ]
    | Assoc def ->
      let local, decl = Env.def_goji_var "root" env in
      format_app
        !^"G.extract_assoc"
        [ format_fun [ decl ] (snd (self # format_extractor def local)) ;
	  arg ]
    | Param _ -> (* FIXME: parametric converters *)
      format_app
        !^"G.extract_identity"
        [ arg ]
    | Abbrv ((_, (path, name)), Default) ->
      format_app (format_ident (path, "extract_" ^ name)) [ arg ]
    | Abbrv (abbrv, Extern (inject, extract)) ->
      format_app (format_ident extract) [ arg ]
    | Callback _ | Handler _ ->
      error "functional extraction not supported yet"
    | Abbrv (abbrv, Custom def) ->
      (* TODO: check *)
      snd (self # format_extractor def env)


  method format_storage_access sto env =
    match sto with
    | Global n -> format_app !^"G.js_global" [ !^!n ]
    | Var n -> Env.use_goji_var n env
    | Arg ("args", n) -> Env.use_goji_var ("args'" ^ string_of_int n) env
    | Arg _ -> failwith "error 1458"
    | Rest _ -> failwith "error 1459"
    | Field (sto, n) ->
      format_app
	!^"G.js_get"
	[ self # format_storage_access sto env ; !^!n ]
    | Cell (sto, i) ->
      format_app
	!^"G.js_get_any "
	[ self # format_storage_access sto env ;
	  !^"G.js_of_int " ^^ int i ]

  (** Constructs a JavaScript value from a constant litteral *)
  method format_const = function
    | Const_int i -> !^(Printf.sprintf "(G.js_of_int %d)" i)
    | Const_float f -> !^(Printf.sprintf "(G.js_of_float %g)" f)
    | Const_bool b -> !^(Printf.sprintf "(G.js_of_bool %b)" b)
    | Const_string s -> !^(Printf.sprintf "(G.js_of_string %S)" s)
    | Const_undefined -> !^(Printf.sprintf "(G.js_constant %S)" "undefined")
    | Const_null -> !^(Printf.sprintf "(G.js_constant %S)" "null")

  (** Compiles a guard to an OCaml boolean expression *)
  method format_guard guard env =
    match guard with
    | True -> !^"true"
    | False -> !^"false"
    | Raise p ->
      format_app !^"raise" [ format_ident p ]
    | Not g ->
      format_app !^"not"
        [ self # format_guard g env ]
    | And (g1, g2) ->
      format_app !^"(&&)"
        [ self # format_guard g1 env ; self # format_guard g2 env ]
    | Or (g1, g2) ->
      format_app !^"(||)"
        [ self # format_guard g1 env ; self # format_guard g2 env ]
    | Const (sto, c) ->
      format_app !^"G.js_equals"
        [ self # format_storage_access sto env ;
          self # format_const c ]

  (* definition generation entry points ***************************************)

  method format_type_definition tparams name type_mapping doc =
    [ format_comment true (self # format_doc doc)
      ^^ group
          (match type_mapping with
           | Typedef (vis, def) ->
             group (!^"type" ^^^ self # format_type_params tparams ^^ !^name ^^^ !^"=")
             ^^^ self # format_value_type def
           | Gen_sym ->
             group (!^"type" ^^^ self # format_type_params tparams ^^ !^name ^^^ !^"= string")
           | Gen_id ->
             group (!^"type" ^^^ self # format_type_params tparams ^^ !^name ^^^ !^"= int")
           | Format -> failwith "format not implemented")
    ] @ [
      match type_mapping with
      | Typedef (vis, def) -> empty
      | Gen_sym ->
        format_comment true (format_words ("Makes a fresh, unique instance of [" ^ name ^ "]."))
        ^^ format_let !^("make_" ^ name)
            (format_let_in !^"uid"
               (format_words "ref 0")
               (format_words "fun () -> incr uid ; \"gg\" ^ string_of_int !uid"))
      | Gen_id ->
        format_comment true (format_words ("Makes a fresh, unique instance of [" ^ name ^ "]."))
        ^^ format_let !^("make_" ^ name)
            (format_let_in !^"uid"
               (format_words "ref 0")
               (format_words "fun () -> incr uid ; !uid"))
      | Format -> failwith "format not implemented"
    ] @ [
      match type_mapping with
      | Typedef (vis, def) ->
        format_hidden
          (self # format_injector_definition name def
           ^^ hardline
           ^^ self # format_extractor_definition name def)
      | Gen_sym ->
        format_hidden
          (self # format_injector_definition name (Value (String, Var "root"))
           ^^ hardline
           ^^ self # format_extractor_definition name  (Value (String, Var "root")))
      | Gen_id ->
        format_hidden
          (self # format_injector_definition name (Value (Int, Var "root"))
           ^^ hardline
           ^^ self # format_extractor_definition name  (Value (Int, Var "root")))
      | Format -> failwith "format not implemented" ]

  method format_method_definition (_, (tpath, tname) as abbrv) name params body ret doc =
    let params =
      [ (Curry, "this",
         Doc ("The [" ^ string_of_ident (tpath, tname) ^ "] instance"),
         Value (Abbrv (abbrv, Default), Var "this"))] @ params
    in
    self # format_function_definition name params body ret doc

  method format_function_definition name params body ret doc =
    let format_param (pt, name, doc, def) =
      let c, def = match pt with
	| Optional -> !^"?", Goji_dsl.option_undefined def
	| Curry -> empty, def
	| Labeled -> !^"~", def
      in
      group (c ^^ format_annot !^name (self # format_value_type def))
    in
    let body =
      let call_sites = self # format_call_sites params body in
      let env, params = self # format_arguments_injection params Env.empty in
      let env, body = self # format_body body env in
      let env, ret =
	match ret with
	| Value (Void, _) when not Env.(exists_goji_var "result" env)->
	  env, []
	| Value (Void, _) ->
	  env, seq_result (format_app !^"ignore" [ Env.use_goji_var "result" env ])
	| _ ->
	  self # format_result_extractor Goji_dsl.(ret @@ Var "result") env
      in
      Env.warn_unused env ;
      format_sequence (call_sites @ params @ body @ ret)
    in
    [ format_comment true (self # format_function_doc doc name params)
      ^^ (format_let
            (format_fun_pat !^name  ~annot:(self # format_value_type ret)
	       (List.map format_param params))
	    body) ]

  method format_inherits_definition name t1 t2 doc =
    let params = [ Curry, "this", Nodoc,
                   Value (Abbrv (t1, Default), Var "temp") ] in
    let ret = Value (Abbrv (t2, Default), Var "root") in
    let body = Access (Var "temp") in
    self # format_function_definition name params body ret doc

  method format_body body env =
    match body with
    | Nop -> env, []
    | Call_method (rsto, name, cs) ->
      let res =
	format_app
	  !^"G.js_call_method"
	  [ self # format_storage_access rsto env ; !^!name ;
	    format_app !^"G.build_args" [ !^(cs ^ "'A") ] ]
      in
      let env, res = Env.let_goji_var "result" ~ro:false res env in
      env, seq_instruction' res
    | Call (fsto, cs) ->
      let res =
	format_app
	  !^"G.js_call"
	  [ self # format_storage_access fsto env ;
	    format_app !^"G.build_args" [ !^(cs ^ "'A") ] ]
      in
      let env, res = Env.let_goji_var "result" ~ro:false res env in
      env, seq_instruction' res
    | New (csto, cs) ->
      let res =
	format_app
	  !^"G.js_call_constructor"
	  [ self # format_storage_access csto env ;
	    format_app !^"G.build_args" [ !^(cs ^ "'A") ] ]
      in
      let env, res = Env.let_goji_var "result" ~ro:false res env in
      env, seq_instruction' res
    | Access sto ->
      let res = self # format_storage_access sto env in
      let env, res = Env.let_goji_var "result" ~ro:false res env in
      env, seq_instruction' res
    | Inject_constants asses ->
      List.fold_right
	(fun (c, sto) (env, r) ->
	  let c = (self # format_const c) in
	  let env, ass = self # format_storage_assignment c sto env in
	  env, ass @ r)
	asses (env, [])
    | Abs (n, v, b) ->
      let envi, resi = self # format_body v env in
      let nvars = Env.goji_vars_diff env envi in
      if nvars <> [] then
	let ndecls = List.map (function "result" -> n | o -> o) nvars in
	let reti = format_tuple (List.map (fun v -> Env.use_goji_var n envi) nvars) in
	let resi = resi @ seq_instruction reti in
	let env =
	  List.fold_left
	  (* this is a horrid hack, thank me very much if you have to read this *)
	    (fun env v -> fst (Env.let_goji_var ~used:true ~ro:false v !^v env))
	    env ndecls
	in
	let envb, resb =  self # format_body b env in
	let decls = List.map (!^) ndecls in
	envb, seq_let_in (format_tuple decls) (format_sequence resi) @ resb
      else
	let envb, resb =  self # format_body b env in
	envb, resi @ resb
    | Test (cond, bt, bf) ->
      let envt, rest =  self # format_body bt env in
      let envf, resf =  self # format_body bf env in
      let nvars = Env.(merge_vars (List.map (goji_vars_diff env) [ envt ; envf ])) in
      if nvars <> [] then
	let env =
	  List.fold_left
	    (* this is a horrid hack, thank me very much if you have to read this *)
	    (fun env v -> fst (Env.let_goji_var ~used:true v !^v env))
	    env nvars
	in
	let body =
	  format_if
	    (self # format_guard cond env)
	    (format_sequence (rest @ seq_result (Env.tuple_goji_vars nvars envt)))
	    (format_sequence (resf @ seq_result (Env.tuple_goji_vars nvars envf)))
	in
	env, seq_let_in (format_tuple (List.map (!^) nvars)) body
      else
	env,
	seq_result
	  (format_if
	     (self # format_guard cond env)
	     (format_sequence rest)
	     (format_sequence resf))
    | Inject (var, def) ->
      let var = string_of_ident var in
      let env, _ = Env.def_ocaml_var var env in
      self # format_injector var def env

  method format_call_sites params body =
    let rec collect = function
      | Call_method (_, _, cs) | Call (_, cs) | New (_, cs) -> [ cs ]
      | Access _ | Nop | Inject _ | Inject_constants _ -> []
      | Test (_, b1, b2) | Abs (_, b1, b2) -> collect b1 @ collect b2
    in
    let size n =
      let collect = object (self)
 	inherit [int] collect 0 as mom
	method! storage = function
	  | Arg (cs, i) when cs = n && self # current < i + 1 ->
	    self # store (i + 1)
	  | oth -> mom # storage oth
      end in
      collect # body body ;
      List.iter (collect # parameter) params ;
      collect # result
    in
    List.flatten
      (List.map
	 (fun n ->
            seq_let_in
              !^(n ^ "'A")
              (format_app !^"G.alloc_args" [ int (size n) ]))
	 (collect body))

  (* interface generation entry points *)

  method format_type_interface tparams name type_mapping doc =
    let abbrv = (List.map (fun (_, n) -> Value (Param n, Var "root")) tparams, ([], name)) in
    let abbrv = Value (Abbrv (abbrv, Default), Var "root") in
    let any = Value (Any, Var "root") in
    [ 
      format_comment true (self # format_doc doc)
      ^^ group
          (match type_mapping with
           | Typedef (Public, def) ->
             group (!^"type" ^^^ self # format_type_params tparams ^^ !^name ^^ break 1 ^^ !^"=")
             ^^^ self # format_value_type def
           | Typedef (Private, def) ->
             group (!^"type" ^^^ self # format_type_params tparams ^^ !^name ^^ break 1 ^^ !^"= private")
             ^^^ self # format_value_type def
           | Typedef (Abstract, _) | Gen_sym | Gen_id ->
             group (!^"type" ^^^ self # format_type_params tparams ^^ !^name)
           | Format -> assert false)
    ] @ [
      match type_mapping with
      | Typedef (vis, def) -> empty
      | Gen_sym | Gen_id ->
        format_comment true
	  (format_words ("Makes a fresh, unique instance of [" ^ name ^ "]."))
        ^^ format_val
	    !^("make_" ^ name)
	    (self # format_fun_type
	       [ Curry, "_", Nodoc, Value (Void, Var "root") ]
	       abbrv)
      | Format -> assert false
    ] @ [
      format_hidden
        (format_val
	   !^("inject_" ^ name)
	   (self # format_fun_type [ Curry, "_", Nodoc, abbrv ] any)
         ^^ hardline
	 ^^ format_val
	     !^("extract_" ^ name)
	     (self # format_fun_type [ Curry, "_", Nodoc, any ] abbrv)) ]

  method format_method_interface (_, (tpath, tname) as abbrv) name params ret doc =
    let params =
      [ (Curry, "this",
         Doc ("The [" ^ string_of_ident (tpath, tname) ^ "] instance"),
         Value (Abbrv (abbrv, Default), Var "this"))] @ params
    in
    self # format_function_interface name params ret doc

  method format_function_interface name params ret doc =
    [ format_comment true (self # format_function_doc doc name params)
      ^^ format_val
  	  !^name
          (self # format_fun_type params ret) ]

  method format_inherits_interface name t1 t2 doc =
    let params = [ Curry, "this", Nodoc,
                   Value (Abbrv (t1, Default), Var "this") ] in
    let ret = Value (Abbrv (t2, Default), Var "root") in
    [ format_comment true (self # format_function_doc doc name params)
      ^^ format_val !^name (self # format_fun_type params ret) ]

end
