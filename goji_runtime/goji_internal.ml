open JavaScript

type arg_block = {
  args : any array ;
  mutable rest : any list ; 
}

let alloc_args nb =
  { args = Array.make nb (Ops.constant "undefined") ;
    rest = [] }

let push_arg args arg =
  args.rest <- arg :: args.rest

let set_arg args idx arg =
  args.args.(idx) <- arg

let build_args args =
  if args.rest = [] then
    args.args
  else
    Array.concat
      [ args.args ;
        Array.of_list (List.rev args.rest) ]

let ensure_block_global n =
  let v = Ops.global n in
  if Ops.equals (Ops.constant "undefined") v then
    let b = Ops.obj [| |] in
    Ops.set_global n b ; b
  else v

let ensure_block_var res =
  if Ops.equals (Ops.constant "undefined") res then
    Ops.obj [| |]
  else res

let ensure_block_arg args idx =
  let v = args.args.(idx) in
  if Ops.equals (Ops.constant "undefined") v then
    let b = Ops.obj [| |] in
    args.args.(idx) <- b ; b
  else v

let ensure_block_field o f =
  let v = Ops.get o f in
  if Ops.equals (Ops.constant "undefined") v then
    let b = Ops.obj [| |] in
    Ops.set o f b ; b
  else v

let ensure_block_cell o i =
  let v = Ops.get_any o (Ops.of_int i) in
  if Ops.equals (Ops.constant "undefined") v then
    let b = Ops.obj [| |] in
    Ops.set_any o (Ops.of_int i) b ; b
  else v
