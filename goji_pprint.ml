(****************************************************************************)
(* GOJI (JavaScript Interface Generator for OCaml)                          *)
(* This file is published under the CeCILL licence                          *)
(* (C) 2013 Benjamin Canou                                                  *)
(****************************************************************************)

type impl =
  [ `Mod of string * impl list
  | `Com of com list
  | `Let of string * expr
  | `Exc of string * ty list
  | `Ty
  | `Nl ]
and intf =
  [ `Sig of string * intf list
  | `Com of com list
  | `Val of string * ty
  | `Exc of string * ty list
  | `Ty
  | `Nl ]
and seq =
  [ `Letin of string list * expr
  | `Expr of expr ]
and expr =
  [ `Var of string
  | `Fun of (string * ty option) list * ty option * expr
  | `App of expr * expr list
  | `Match of expr * (expr * expr) list
  | `Tuple of expr list ]
and ty =
  [ `Alias of ty list * string
  | `Prod of ty list ]
and com =
  [ `Section of int * com list
  | `Par of com list
  | `Code_par of string        
  | `Word of string
  | `Code of string ]

let format_impl _ = PPrint.empty

let format_intf _ = PPrint.empty

let split_words text =
  List.map (fun w -> `Word w)
    Str.(split (regexp "[ \t]+") text)
