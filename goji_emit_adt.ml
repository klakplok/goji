(****************************************************************************)
(* GOJI (JavaScript Interface Generator for OCaml)                          *)
(* This file is published under the CeCILL licence                          *)
(* (C) 2013 Benjamin Canou                                                  *)
(****************************************************************************)

(** Back-end for OCaml with methods projected to simple functions over
    Abstract Data Type *)

(* THIS FILE IS A (WORKING) DRAFT *)

open Goji_pprint
open Goji_ast

let fix_params params =
  let fake_unit_param = Curry, "()", Nodoc, Value (Void, Var "<fake>") in
  let params = match params with [] -> [ fake_unit_param ] | p -> p in
  let rec add_fake_positional acc = function
    | (Curry | Labeled), _, _, _ as p1 :: tl ->
      List.rev tl @ acc @ [ p1 ]
    | Optional, _, _, _ as p :: tl ->
      add_fake_positional (p :: acc) tl
    | [] -> List.rev (fake_unit_param :: acc)
  in add_fake_positional [] (List.rev params)

class emitter = object (self)
  inherit Goji_emit_struct.emitter as mommy

  (* common utility methods for interface and definition generation *)

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

  method format_injector_definition name def =
    format_let
      (!^("inject_" ^ name) ^^^ !^"v")
      (self # format_injector_body "v" def)

  method format_extractor_definition name def =
    format_let
      (!^("extract_" ^ name) ^^^ !^"root")
      (self # format_extractor_body def)

  method format_injector_body var def =
    let def = Goji_dsl.(def @@ Var "res") in
    format_let_in !^"res'V" !^^"ref (JavaScript.Ops.constant \"undefined\")"
      (format_sequence
	 (self # format_injector var def
	  @ seq_instruction !^"!res'V"))

  method format_extractor_body def =
    self # format_extractor def

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
    | Value (Event_setter (_, params, ret), _)
    | Value (Event_canceller (_, params, ret), _)
    | Value (Callback (params, ret), _) ->
      (if sa then !^"(" else empty)
      ^^ align (self # format_fun_type params ret)
      ^^ (if sa then !^")" else empty)

  (** Constructs the OCaml arrow type from the defs of parameters and
      return value. Does not put surrounding parentheses. *)
  method format_fun_type params ret =
    let params = fix_params params in
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
    let params = fix_params params in
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

  (* definition generation entry points *)

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
          (self # format_injector_definition name (Value (String, Var "<root>"))
           ^^ hardline
           ^^ self # format_extractor_definition name  (Value (String, Var "<root>")))
      | Gen_id ->
        format_hidden
          (self # format_injector_definition name (Value (Int, Var "<root>"))
           ^^ hardline
           ^^ self # format_extractor_definition name  (Value (Int, Var "<root>")))
      | Format -> failwith "format not implemented" ]

  method format_method_definition (_, (tpath, tname) as abbrv) name params body ret doc =
    let params =
      [ (Curry, "this",
         Doc ("The [" ^ string_of_ident (tpath, tname) ^ "] instance"),
         Value (Abbrv (abbrv, Default), Var "<this>"))] @ params
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
    let params = fix_params params in
    [ format_comment true (self # format_function_doc doc name params)
      ^^ format_let
          (format_fun_pat
	     !^name
	     ~annot:(self # format_value_type ret)
	     (List.map format_param params))
          (format_sequence
	     (self # format_call_sites params body
	      @ self # format_temp_vars params body
              @ self # format_arguments_injection params
	      @ seq_let_in !^"result" (self # format_body body)
              @ seq_instruction (self # format_extractor Goji_dsl.(ret @@ Var "<result>")))) ]

  method format_inherits_definition name t1 t2 doc =
    let params = [ Curry, "this", Nodoc,
                   Value (Abbrv (t1, Default), Var "temp") ] in
    let ret = Value (Abbrv (t2, Default), Var "<root>") in
    let body = Access (Var "temp") in
    self # format_function_definition name params body ret doc

  method format_arguments_injection params =
    let format_param (pt, name, _, def) =
      let def = match pt with
	| Optional -> Goji_dsl.option_undefined def
        | Curry | Labeled -> def
      in
      self # format_injector name def
    in
    List.flatten (List.map format_param params)

  method format_body = function
    | Nop -> !^"()"
    | Call_method (rsto, name, cs) ->
      format_app
	!^"JavaScript.Ops.call_method"
	[ self # format_storage_access rsto ; !^!name ;
	  format_app !^"Goji_internal.build_args" [ !^(cs ^ "'A") ] ]
    | Call (fsto, cs) ->
      format_app
	!^"JavaScript.Ops.call"
	[ self # format_storage_access fsto ;
	  format_app !^"Goji_internal.build_args" [ !^(cs ^ "'A") ] ]
    | New (csto, cs) ->
      format_app
	!^"JavaScript.Ops.call_constructor"
	[ self # format_storage_access csto ;
	  format_app !^"Goji_internal.build_args" [ !^(cs ^ "'A") ] ]
    | Access sto ->
      self # format_storage_access sto
    | Inject_constants asses ->
      List.fold_left
	(fun r (c, sto) ->
	  r ^^ format_let_in !^"v" (self # format_const c)
	         (format_sequence (self # format_storage_assignment !^"v" sto)))
	empty asses
    | Inject _ -> assert false
    | Abs (n, v, b) ->
      format_let_in
	!^n
	(self # format_body v)
	(self # format_body b)
    | Test (cond, bt, bf) ->
      format_if
	(self # format_guard cond)
	(self # format_body bt)
	(self # format_body bf)

  method format_storage_access = function
    | Global n -> format_app !^"JavaScript.Ops.global" [ !^!n ]
    | Var n when n.[0] = '<' -> !^(String.sub n 1 (String.length n - 2))
    | Var n -> !^("!" ^ n ^ "'V")
    | Arg ("args", n) -> !^"args'" ^^ int n (* TODO: check if in callback position *)
    | Arg _ -> failwith "error 1458"
    | Rest _ -> failwith "error 1459"
    | Field (sto, n) ->
      format_app
	!^"JavaScript.Ops.get"
	[ self # format_storage_access sto ; !^!n ]
    | Cell (sto, i) ->
      format_app
	!^"JavaScript.Ops.get_any "
	[ self # format_storage_access sto ;
	  !^"JavaScript.Ops.of_int " ^^ int i ]

  method format_storage_assignment v sto =
    let rec toplevel sto =
      match sto with
      | Global n ->
	seq_instruction (format_app !^"JavaScript.Ops.set_global" [ !^!n ; v ])
      | Var "<this>" ->
	seq_let_in !^"this" v
      | Var n when n.[0] = '<' ->
	seq_instruction empty
      (*      | Var n when n.[0] = '<' -> format_let_in !^(String.sub n 1 (String.length n - 2)) v empty *)
      | Var n ->
	seq_instruction (format_ass !^(n ^ "'V") v)
      | Arg (cs, n) ->
	seq_instruction (format_app !^"Goji_internal.set_arg"
			   [ !^(cs ^ "'A") ; int n ; v ])
      | Rest cs ->
	seq_instruction (format_app !^"Goji_internal.push_arg"
			   [ !^(cs ^ "'A") ; v ])
      | Field (sto, n) ->
	seq_instruction (format_app
			   !^"JavaScript.Ops.set"
			   [ nested sto ; !^!n ; v ])
      | Cell (sto, i) ->
        seq_instruction
	  (format_app
	     !^"JavaScript.Ops.set_any "
	     [ nested sto ;
	       !^"(JavaScript.Ops.of_int" ^^^ !^(string_of_int i) ^^ !^")" ; v ])
    and nested sto =
      match sto with
      | Rest cs -> failwith "error 4679"
      | Global n ->
        format_app !^"Goji_internal.ensure_block_global" [ !^!n ]
      | Var n when n.[0] = '<' ->
        (* FIXME: insert check *)
        !^(String.sub n 1 (String.length n - 2))
      | Var n ->
        format_app !^"Goji_internal.ensure_block_var" [ !^(n ^ "'V") ]
      | Arg (cs, n) ->
        format_app !^"Goji_internal.ensure_block_arg" [ !^(cs ^ "'A") ; int n ]
      | Field (sto, n) ->
        format_app !^"Goji_internal.ensure_block_field" [ nested sto ; !^!n ]
      | Cell (sto, i) ->
        format_app !^"Goji_internal.ensure_block_cell" [ nested sto ; int i ]
    in toplevel sto

  (** Constructs a JavaScript value from a constant litteral *)
  method format_const = function
    | Const_int i -> !^(Printf.sprintf "(JavaScript.Ops.of_int %d)" i)
    | Const_float f -> !^(Printf.sprintf "(JavaScript.Ops.of_float %g)" f)
    | Const_bool b -> !^(Printf.sprintf "(JavaScript.Ops.of_bool %b)" b)
    | Const_string s -> !^(Printf.sprintf "(JavaScript.Ops.of_string %S)" s)
    | Const_undefined -> !^(Printf.sprintf "(JavaScript.Ops.constant %S)" "undefined")
    | Const_null -> !^(Printf.sprintf "(JavaScript.Ops.constant %S)" "null")

  (** Compiles a guard to an OCaml boolean expression *)
  method format_guard = function
    | True -> !^"true"
    | False -> !^"false"
    | Raise p ->
      format_app !^"raise" [ format_ident p ]
    | Not g ->
      format_app !^"not"
        [ self # format_guard g ]
    | And (g1, g2) ->
      format_app !^"(&&)"
        [ self # format_guard g1 ; self # format_guard g2 ]
    | Or (g1, g2) ->
      format_app !^"(||)"
        [ self # format_guard g1 ; self # format_guard g2 ]
    | Const (sto, c) ->
      format_app !^"JavaScript.Ops.equals"
        [ self # format_storage_access sto ;
          self # format_const c ]

  (** produces code that injects the OCaml variable [v] of structure
      [def] in the context *)
  method format_injector ?(path = []) v def =
    match def with
    | Record fields ->
      List.flatten
        (List.map
           (fun (n, def, doc) ->
              seq_let_in
                !^("f'" ^ n)
                (!^v ^^ !^"." ^^ format_ident (path, n))
              @ self # format_injector ("f'" ^ n) def)
           fields)
    | Variant cases ->
      seq_instruction
	(format_match !^v
	   (List.map
              (fun (n, g, defs, doc) ->
		if defs = [] then
		  !^n, empty (* FIXME: THIS SUCKS !*)
		else
		  (!^n ^^ !^" "
		   ^^ format_tuple (List.mapi (fun i _ -> !^("vc'" ^ string_of_int i)) defs),
		   format_sequence
		     (List.flatten
			(List.mapi
			   (fun i def ->
			     self # format_injector ("vc'" ^ string_of_int i) def)
			   defs))))
	      cases))
    | Tuple (defs) ->
      seq_instruction
        (let vars = List.mapi (fun i def -> ("v'" ^ string_of_int i, def)) defs in
         let seq =
           seq_let_in (format_tuple (List.map (fun (v, _) -> !^v) vars)) !^v
           @ List.flatten (List.map (fun (v, def) -> self # format_injector v def) vars)
         in !^"begin" ^^ nest 2 (hardline ^^ format_sequence seq) ^^ hardline ^^ !^"end")
    | Option (g, d) ->
      seq_instruction
	(format_match !^v
           [ !^"Some v", format_sequence (self # format_injector "v" Goji_dsl.(d)) ;
             !^"None", self # format_guard_injector g ])
    | Value (leaf, sto) ->
      self # format_storage_assignment
        (self # format_leaf_injector v leaf)
        sto

  method format_guard_injector g =
    let rec collect = function
      | Const (sto, c) -> [ (sto, c) ]
      | Not _ -> []
      | And (g1, g2) -> collect g1 @  collect g2
      | Or (g, _) -> collect g
      | Raise _ | True | False -> []
    in
    let asses = collect g in
    format_sequence
      (List.flatten
	 (List.map (fun (sto, c) ->
	   self # format_storage_assignment
	     (self # format_const c)
	     sto)
	    asses))

  method format_leaf_injector v leaf =
    match leaf with
    | Int -> format_app !^"JavaScript.Inject.int" [ !^v ]
    | String -> format_app !^"JavaScript.Inject.string" [ !^v ]
    | Bool -> format_app !^"JavaScript.Inject.bool" [ !^v ]
    | Float -> format_app !^"JavaScript.Inject.float" [ !^v ]
    | Any -> !^v
    | Void ->
      !^("(ignore " ^ v ^ " ; JavaScript.Ops.constant \"undefined\")")
    | Array def ->
      format_app
        !^"JavaScript.Inject.array"
        [ group (!^^"(fun v ->" ^^ (nest 2 (break 1 ^^ self # format_injector_body "v" def)) ^^ !^")") ;
          !^v ]
    | List def -> format_app !^"Array.to_list" [ self # format_leaf_injector v (Array def) ]
    | Assoc def ->
      format_app
        !^"JavaScript.Inject.assoc"
        [ group (!^^"(fun v ->" ^^ (nest 2 (break 1 ^^ self # format_injector_body "v" def)) ^^ !^")") ;
          !^v ]
    | Param _ -> (* FIXME: check all this magic ? *)
      format_app
        !^"JavaScript.Inject.identity"
        [ !^v ]
    | Abbrv ((_, (path, name)), Default) ->
      format_app (format_ident (path, "inject_" ^ name)) [ !^v ]
    (* FIXME: add injector params *)
    | Abbrv (abbrv, Extern (inject, extract)) ->
      format_app (format_ident inject) [ !^v ]
    | Callback (params, ret)
    | Event_setter (_, params, ret)
    | Event_canceller (_, params, ret) ->
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
      let body =
	let format_arg (pt, name, _, def) =
	  match pt with
	  | Optional -> failwith "error 1887"
	  | Curry -> self # format_extractor def
	  | Labeled -> !^"~" ^^ !^name ^^ !^":" ^^ self # format_extractor def
	in
	format_let_in !^"cbres"
	  (format_app !^v
	     (if params = [] then [ !^"()" ]
	      else List.map format_arg params))
	  (self # format_injector_body "cbres" ret)
      in
      format_app !^"JavaScript.Ops.wrap_fun"
	[ group (!^"(fun"
		 ^^ (if max_arg = 0 then
		       !^" () ->"
		     else
		       let rec params i =
			 if i = 0 then empty
			 else params (i - 1) ^^ !^" args'" ^^ int (i - 1)
		       in params max_arg ^^ !^" ->")
		 ^^ group (nest 2 (break 1 ^^ body))
		 ^^ !^")") ]
    | Abbrv (abbrv, Custom def) -> !^"(assert false)"

  (** produces code that extracts an OCaml value of structure [def]
      from the context *)
  method format_extractor def =
    match def with
    | Record fields ->
      format_record
        (List.map
           (fun (n, def, doc) -> !^n, self # format_extractor def)
           fields)
    | Variant cases ->
      List.fold_right
        (fun (n, g, defs, doc) alt ->
           format_if
             (self # format_guard g)
             (if defs = [] then
		 !^n
	      else
		 !^n ^^ (nest 2 (break 1
				 ^^ format_tuple
				   (List.map (self # format_extractor) defs))))
             alt)
        cases
        !^("failwith \"unable to extract\"" (* FIXME: type name *))
    | Tuple (defs) -> format_tuple (List.map (self # format_extractor) defs)
    | Option (g, d) ->
      format_if
        (self # format_guard g)
        !^"None"
        (format_app !^"Some "[ self # format_extractor d ])
    | Value (leaf, sto) ->
      self # format_leaf_extractor leaf
        (self # format_storage_access sto)

  method format_leaf_extractor leaf sto =
    match leaf with
    | Int -> format_app !^"JavaScript.Extract.int" [ sto ]
    | String -> format_app !^"JavaScript.Extract.string" [ sto ]
    | Bool -> format_app !^"JavaScript.Extract.bool" [ sto ]
    | Float -> format_app !^"JavaScript.Extract.float" [ sto ]
    | Any -> sto
    | Void -> format_app !^"ignore" [ sto ]
    | Array def ->
      format_app
        !^"JavaScript.Extract.array"
        [ group (!^^"(fun root ->" ^^ (nest 2 (break 1 ^^ self # format_extractor def)) ^^ !^")") ;
          sto ]
    | List def -> format_app !^"Array.to_list" [ self # format_leaf_extractor (Array def) sto ]
    | Assoc def ->
      format_app
        !^"JavaScript.Extract.assoc"
        [ group (!^^"(fun root ->" ^^ (nest 2 (break 1 ^^ self # format_extractor def)) ^^ !^")") ;
          sto ]
    | Param _ -> (* FIXME: check all this magic ? *)
      format_app
        !^"JavaScript.Extract.identity"
        [ sto ]
    | Abbrv ((_, (path, name)), Default) ->
      format_app (format_ident (path, "extract_" ^ name)) [ sto ]
    (* FIXME: add injector params *)
    | Abbrv (abbrv, Extern (inject, extract)) ->
      format_app (format_ident extract) [ sto ]
    | Callback _
    | Event_setter _
    | Event_canceller _ -> !^"(assert false)"
    | Abbrv (abbrv, Custom def) -> !^"(assert false)"

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
              (format_app !^"Goji_internal.alloc_args" [ int (size n) ]))
	 (collect body))

  method format_temp_vars params body =
    let vars =
      let module SS = Set.Make (String) in
      let collect = object (self)
	inherit [SS.t] collect SS.empty as mom
	method! storage = function
	  | Var n when n.[0] <> '<' ->
	    self # store (SS.add n (self # current))
	  | oth -> mom # storage oth
      end in
      collect # body body ;
      List.iter (collect # parameter) params ;
      SS.elements (collect # result)
    in
    List.flatten
      (List.map
	 (fun n ->
	    seq_let_in !^^(n ^ "'V")
	      !^^"ref JavaScript.Ops.undefined")
	 vars)

  (* interface generation entry points *)

  method format_type_interface tparams name type_mapping doc =
    let abbrv = (List.map (fun (_, n) -> Value (Param n, Var "<root>")) tparams, ([], name)) in
    let abbrv = Value (Abbrv (abbrv, Default), Var "<root>") in
    let any = Value (Any, Var "<root>") in
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
	       [ Curry, "_", Nodoc, Value (Void, Var "<root>") ]
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
         Value (Abbrv (abbrv, Default), Var "<this>"))] @ params
    in
    self # format_function_interface name params ret doc

  method format_function_interface name params ret doc =
    let params = fix_params params in
    [ format_comment true (self # format_function_doc doc name params)
      ^^ format_val
  	  !^name
          (self # format_fun_type params ret) ]

  method format_inherits_interface name t1 t2 doc =
    let params = [ Curry, "this", Nodoc,
                   Value (Abbrv (t1, Default), Var "temp") ] in
    let ret = Value (Abbrv (t2, Default), Var "<root>") in
    [ format_comment true (self # format_function_doc doc name params)
      ^^ format_val !^name (self # format_fun_type params ret) ]

end
