open Position
open Error
open FopixAST
       

(** [error pos msg] reports runtime error messages. *)
let error positions msg =
  errorN "execution" positions msg
   
let ( >>= ) m f =
  match m with
    | None -> None
    | Some x -> f x

let return x =
  Some x

let exit_m = function
  | None -> assert false (* Impossible. *)
  | Some x -> x  
       
(** Every expression of fopi evaluates into a [value]. *)
type value =
  | VUnit
  | VInt       of Int32.t
  | VBool      of bool
  | VChar      of char
  | VString    of string
  | VAddress   of Bmemory.address
  | VFun       of function_identifier
  | VPrimitive of string * (value list -> value)

type 'a coercion = value -> 'a option
let value_as_int      = function VInt x -> Some x | _ -> None
let value_as_bool     = function VBool x -> Some x | _ -> None
let value_as_address  = function VAddress x -> Some x | _ -> None
let value_as_unit     = function VUnit -> Some () | _ -> None

type 'a wrapper = 'a -> value
let int_as_value x  = VInt x
let bool_as_value x = VBool x
let address_as_value x = VAddress x
let unit_as_value () = VUnit

let print_value m v =
  let max_depth = 5 in

  let rec print_value d v =
    if d >= max_depth then "..." else
      match v with
        | VInt x ->
          Int32.to_string x
        | VBool true ->
          "true"
        | VBool false ->
          "false"
  | VChar c ->
    "'" ^ Char.escaped c ^ "'"
  | VString s ->
    "\"" ^ String.escaped s ^ "\""
  | VUnit ->
    "()"
  | VAddress a ->
    print_block m d a
  | VFun _ ->
    "<fun>"
        | VPrimitive (s, _) ->
           Printf.sprintf "<primitive: %s>" s

  and print_block m d a =
    let vs = Array.to_list (Bmemory.read_block m a) in
    "[ " ^ String.concat "; " (List.map (print_value (d+1)) vs) ^ " ]"
  in
  print_value 0 v

module Environment : sig
  type t
  val initial : t
  val bind    : t -> identifier -> value -> t
  exception UnboundIdentifier of identifier
  val lookup  : identifier -> t -> value
  val last    : t -> (identifier * value * t) option
  val print   : value Bmemory.t -> t -> string
end = struct
  type t = (identifier * value) list

  let initial = []

  let bind e x v = (x, v) :: e

  exception UnboundIdentifier of identifier

  let lookup x e =
    try
      List.assoc x e
    with Not_found ->
      raise (UnboundIdentifier x)

  let last = function
    | [] -> None
    | (x, v) :: e -> Some (x, v, e)

  let print_binding memory (Id x, v) =
    (* Identifiers starting with '_' are reserved for the compiler.
       Their values must not be observable by users. *)
    if x.[0] = '_' then
      ""
    else
      x ^ " = " ^ print_value memory v

  let print memory env =
    String.concat "\n" (
      List.(filter (fun s -> s <> "") (map (print_binding memory) env))
    )

end

type runtime = {
  environment : Environment.t;
  functions   : (function_identifier * (formals * expression)) list;
}

type observable = {
  new_environment : Environment.t;
}

let initial_runtime () = {
  environment = Environment.initial;
  functions  = [];
}

let memory : value Bmemory.t ref = ref (Bmemory.fresh ())

let rec evaluate runtime ast =
  let runtime = List.fold_left bind_function runtime ast in
  let runtime' = List.fold_left declaration runtime ast in
  (runtime', extract_observable runtime runtime')

and bind_function runtime = function
  | DefineValue _ ->
    runtime

  | DefineFunction (f, xs, e) ->
    { runtime with
      functions = (f, (xs, e)) :: runtime.functions
    }

  | ExternalFunction _ ->
    runtime (* FIXME: bind to internal primitives later. *)

and declaration runtime = function
  | DefineValue (i, e) ->
    let v = expression runtime e in
    { runtime with environment = Environment.bind runtime.environment i v }
  | DefineFunction _ ->
    runtime
  | ExternalFunction _ ->
    runtime

and arith_operator_of_symbol = function
  | "`+" -> Int32.add
  | "`-" -> Int32.sub
  | "`/" -> Int32.div
  | "`*" -> Int32.mul
  | _ -> assert false

and cmp_operator_of_symbol = function
  | "`<" -> ( < )
  | "`>" -> ( > )
  | "`<=" -> ( <= )
  | "`>=" -> ( >= )
  | "`=" -> ( = )
  | _ -> assert false

and boolean_operator_of_symbol = function
  | "`&&" -> ( && )
  | "`||" -> ( || )
  | _ -> assert false

and evaluation_of_binary_symbol environment = function
  | ("`+" | "`-" | "`*" | "`/") as s ->
    arith_binop environment (arith_operator_of_symbol s)
  | ("`<" | "`>" | "`<=" | "`>=" | "`=") as s ->
    arith_cmpop environment (cmp_operator_of_symbol s)
  | ("||" | "&&") as s ->
    boolean_binop environment (boolean_operator_of_symbol s)
  | _ -> assert false

and is_binary_primitive = function
  | "`+" | "`-" | "`*" | "`/" | "`<" | "`>" | "`<=" | "`>=" | "`="
  | "`&&" | "`||" -> true
  | _ -> false

and expression runtime = function
  | Literal l ->
     literal l

  | Variable x ->
     let res = Environment.lookup x runtime.environment in
     res

  | Switch (e, bs, default) ->
    begin match value_as_int (expression runtime e) with
      | None -> error [] "Switch on integers only."
      | Some i ->
  let i = Int32.to_int i in
  if i < Array.length bs then
    expression runtime bs.(i)
  else match default with
    | Some t -> expression runtime t
    | None -> error [] "No default case in switch."
    end

  | IfThenElse (c, t, f) ->
     begin
       match expression runtime c with
       | VBool b ->
    if b then
      expression runtime t
    else
      expression runtime f
       | _ -> assert false
     end

  | Define (x, ex, e) ->
    let v = expression runtime ex in
    let runtime = { runtime with
      environment = Environment.bind runtime.environment x v
    }
    in
    expression runtime e

  | FunCall (FunId "allocate_block", [size]) ->       
     begin 
       match (expression runtime size) with
       | VInt size' ->
    let a, memory' = Bmemory.allocate !memory size' (VInt Int32.zero) in
    memory := memory' ;
    VAddress a
       | _ -> assert false 
     end
       
  | FunCall (FunId "read_block", [location; index]) ->
     let b' = expression runtime location in
     (value_as_address b' >>= fun b ->
      let i' = expression runtime index in
      value_as_int i' >>= fun i ->
      return (Bmemory.read !memory b i)) |> exit_m

  | FunCall (FunId "write_block", [location; index; e]) ->
     let b' = expression runtime location in
     (value_as_address b' >>= fun b ->
      let i' = expression runtime index in
      value_as_int i' >>= fun i ->
      let e' = expression runtime e in 
      let _ = memory :=  Bmemory.write !memory b i e' in
      return VUnit
     ) |> exit_m
     
    
  | FunCall (FunId s, [e1; e2]) when is_binary_primitive s ->
    evaluation_of_binary_symbol runtime s e1 e2
        
  | UnknownFunCall(e,l) ->
     let vfun' = expression runtime e in
     let (args,corps) =
       match vfun' with
       | VFun (FopixAST.FunId x) ->
	  (List.assoc (FopixAST.FunId x) runtime.functions)
       | _ -> ([],e)
     in
     let runtime' = bind_funcall runtime args l in 
     expression runtime' corps
       
        
  | FunCall(FunId s, l)->
     let (arguments,corps) = (List.assoc (FunId s) runtime.functions) in
     let runtime' = bind_funcall runtime arguments l in 
     expression runtime' corps

and bind_funcall runtime arguments values =
  if List.length arguments  = List.length values then 
	 List.fold_left2 (fun run arg exp ->
			  let value = expression run exp in
			  { runtime with
			    environment = Environment.bind run.environment arg value
			  }
			 ) runtime arguments values
  else
    failwith "error not same arguments"

and binop
: type a b. a coercion -> b wrapper -> _ -> (a -> a -> b) -> _ -> _ -> value
= fun coerce wrap runtime op l r ->
  let lv = expression runtime l
  and rv = expression runtime r in
  match coerce lv, coerce rv with
    | Some li, Some ri ->
      wrap (op li ri)
    | _, _ ->
      error [] "Invalid binary operation."

and arith_binop env = binop value_as_int int_as_value env
and arith_cmpop env = binop value_as_int bool_as_value env
and boolean_binop env = binop value_as_bool bool_as_value env

and literal = function
  | LInt x -> VInt x
  | LString s -> VString s
  | LChar c -> VChar c
  | LBool b -> VBool b
  | LFun f -> VFun f

and extract_observable runtime runtime' =
  let rec substract new_environment env env' =
    if env == env' then new_environment
    else
      match Environment.last env' with
        | None -> assert false (* Absurd. *)
        | Some (x, v, env') ->
          let new_environment = Environment.bind new_environment x v in
          substract new_environment env env'
  in
  {
    new_environment =
      substract Environment.initial runtime.environment runtime'.environment
  }

let print_observable runtime observation =
  Environment.print !memory observation.new_environment
