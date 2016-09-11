(* This module implements a compiler from Retrolix to Mips *)

let error pos msg =
  Error.error "compilation" pos msg

(** As in any module that implements {!Compilers.Compiler}, the source
    language and the target language must be specified. *)
module Source = Retrolix
module Target = Mips

type environment = unit

let initial_environment () = ()


module S = Source.AST
module T = Target.AST

	     
let lit i =  (T.Literal (Int16.of_int i))
let spl i = (T.RegisterOffsetAddress(MipsArch.Sp,lit i))
let mipslab = function S.Label x -> T.Label x



					    
(** The labels of global variable are prefixed by __global__. *)
let global_variable_label x =
  "__global__" ^ x

(** [translate p env] turns a Retrolix program into a MIPS program. *)
let rec translate (p : S.t) (env : environment) : T.t * environment =
  
  
  (** [block stacksize locals instructions] compiles a retrolix block
      into a MIPS block. *)
  let rec block stacksize locals instructions =
    let env = List.mapi (fun i x -> (x, i)) locals in
    List.map (fun (S.Label l, i) ->
              {
                T.label = T.Label l;
                T.value =
		  let ins = instruction stacksize locals env l i in
		  
		  ins
		    
              }) instructions

	     
  (** ne pas oublier de gérer le cas à plus de 3 arguments *)
	     
  and load_args stacksize env i = function
    | [] ->
       []
    | a::q ->
       begin
	 match a with 
	 | `Variable ((S.Id s) as  x) ->
	    (load_variable stacksize env (MipsArch.A(i)) x)@
	      (load_args stacksize env (i+1) q)
	 (*** by construction this case should be not arrive**)
	 | _ ->
	    
	    assert false
       end
  and rest_args stacksize env i = function
    | [] ->
       []
    | a::q ->
       begin
	 match a with 
	 | `Variable ((S.Id s) as  x) ->
	    (store_variable stacksize env x (MipsArch.A(i)) )@
	      (rest_args stacksize env (i+1) q)
	 (*** by construction this case should be not arrive**)
	 | _ ->
	    
	    assert false
       end	 

  and util stacksize env ldest lval args =
    fun x ->

    (** procedure récursivent *)
    [T.Addiu(MipsArch.Sp,MipsArch.Sp,lit (-4) );
     T.Sw(MipsArch.Ra,
	  (spl 0)) ]@
      (**(load_args stacksize env (0) args)@**)
      rest_args stacksize env 0 args @
	[T.Jalr x]@
	  (load_args stacksize env (0) args)@
	    (**(restore_args stacksize env 0 args)@**)
	    [T.Lw(MipsArch.Ra,
		  spl 0);
	     T.Addiu(MipsArch.Sp,MipsArch.Sp,lit 4)
	    ]@
	      [T.Move (ldest,MipsArch.V(0))]@
		(filter stacksize env ldest lval [])
		  
  (**
	 
 [instruction stacksize locals env l i] compiles the retrolix
      instruction [i] whose label is [l] into a list of MIPS
      instructions. [stacksize] is the size of the current stack
      frame and [locals] the list of local variables. *)	     
  and instruction stacksize locals env l = function
      
    | S.Call(lval,rval,rvalList) ->
       
       let ldest,var = (ldest_ofsp lval) in
       (match var with 
	  Some x ->
	  (load_args stacksize env (0) rvalList)@	      
	    load_rvalue stacksize env rval ldest (util stacksize env ldest lval rvalList)		       
	| None -> 
	   load_rvalue stacksize env rval ldest (util stacksize env ldest lval [])

		       
       )

    (**call stacksize env f rs**)			 
    | S.TailCall(rval,rvalList) ->
       failwith "extension" 
		
    | S.Ret(x1) ->
       load_rvalue stacksize env x1 tmp1
		   (fun x -> [T.Move((MipsArch.V(0),x))])

    | S.Assign(lval,op,rvalList) ->
       (op_conv stacksize locals env op rvalList lval)
	 
    | S.Jump(lab) -> 
       [T.J(mipslab lab)]
    | S.ConditionalJump(cond,rvalList,lab1,lab2) ->
       (** ici tmp1 n'est jamais utilisé c'est seulment pour remplir les arguments*)
       (condition stacksize env cond rvalList lab1 0 inutile)@[T.J (mipslab lab2)]
								
    | S.Switch(rval,labAr,labOpt) -> 
       failwith "extension" 
    | S.Comment(s) ->
       [T.Comment(s)]
    | S.Exit ->
       let labl = (T.Label "exit" ) in
       (load_rvalue stacksize env (`Immediate (S.LInt Int32.zero)) (tmp1)
		    (fun x -> [T.Move(MipsArch.A(0),x)] ))@
	 [ T.Jal labl]
	   

  and inutile = `Variable (S.Id "e") 

  and rdest_of = function     
    | (`Variable(x))->
       tmp1
    | `Register  x ->
       register x
    | `Immediate l -> failwith ""
			       
  and ldest_ofsp = function     
    | (`Variable(x))->
       tmp1,Some x
    | `Register  x ->
       register x,None
		    
  and ldest_of = function     
    | (`Variable(x))->
       tmp1
    | `Register  x ->
       register x
		
  and condition stacksize env op vs l i rdest =
    match op, vs with
    | S.GT, [ x1;  x2 ] ->
       let ldest = (ldest_of rdest) in
       mk_operation stacksize env ldest x1 x2
		    (fun x y -> if x > y then 1 else 0)
		    (fun x1 x2->
		     [T.Sgt(ldest,x1,x2)]
		     @(filter stacksize env ldest rdest [])
		    ) 
		    
    | S.LT, [  x1;  x2 ] ->
       let ldest = (ldest_of rdest) in
       mk_operation stacksize env ldest x1 x2
		    (fun x y -> if x < y then 1 else 0)
		    (fun x1 x2-> [T.Slt(ldest,x1,x2)]
				 @(filter stacksize env ldest rdest [])) 
		    
    | S.GTE, [  x1;  x2 ] ->     
       let ldest = (ldest_of rdest) in
       mk_operation stacksize env ldest x1 x2
		    (fun x y -> if x >= y then 1 else 0)
		    (fun x1 x2-> [T.Sge(ldest,x1,x2)]
				 @(filter stacksize env ldest rdest [])) 
		    
    | S.LTE, [  x1;  x2 ] ->     
       let ldest = (ldest_of rdest) in
       mk_operation stacksize env ldest x1 x2
		    (fun x y -> if x <= y then 1 else 0)
		    (fun x1 x2-> [T.Sle(ldest,x1,x2)]
				 @(filter stacksize env ldest rdest [])) 
		    
    | S.EQ, [  x1;  x2 ] when i = 2  ->       
       let ldest = (ldest_of rdest) in
       mk_operation stacksize env ldest x1 x2
		    (fun x y -> if x = y then 1 else 0)
		    (fun x1 x2-> [T.Seq(ldest,x1,x2)]
				 @(filter stacksize env ldest rdest [])) 
    | S.EQ, [  x1;  x2 ] when i = 0 ->
       load_rvalue stacksize env x1 (rdest_of x1)
		   (fun x ->  [T.Beqz ((rdest_of x1),mipslab l)])
		   
    | _,  _ -> assert false

		      
  and filter stacksize env reg l instr=
    match l with
    | `Variable x' ->
       store_variable stacksize env x' reg (** Si c'est une variable il faut empiler la valeur du registre tmp*)
    | `Register r ->
       []@instr(*** Rien à faire puisque la valeur est dans le registre de déstination **)
  and post_procc stacksize env rdest x1 l =
    fun reg ->
    match x1 with
    | `Variable x ->
       filter stacksize env reg l []
    | `Register r ->
       filter stacksize env reg l [T.Move(rdest,reg)]
    | `Immediate _ ->
       filter stacksize env reg l []

  and op_conv stacksize local env op vs l =
    match op, vs with
    | S.Load, [ x1 ]->
       let rdest = ldest_of l in
       load_rvalue stacksize env x1 rdest
		   (post_procc stacksize env rdest x1 l)

    | S.Add,  [ x1;  x2 ] ->
       let ldest = (ldest_of l) in
       mk_operation stacksize env ldest x1 x2 (+)
		    (fun x1 x2-> [T.Add(ldest,x1,x2) ]@(filter stacksize env ldest l [])  )
		    
    | S.Mul, [ x1;  x2 ] ->

       let ldest = (ldest_of l) in
       mk_operation stacksize env ldest x1 x2
		    (fun x y -> x * y)
		    (fun x1 x2-> [T.Mul(ldest,x1,x2)]@(filter stacksize env ldest l []))
		    
    | S.Div, [ x1;  x2 ] ->
       
       let ldest = (ldest_of l) in
       mk_operation stacksize env ldest x1 x2 (/)
		    (fun x1 x2-> [T.Div(ldest,x1,x2)]@(filter stacksize env ldest l []))
		    
    | S.Sub, [ x1;  x2 ] -> 
       
       let ldest = (ldest_of l) in
       mk_operation stacksize env ldest x1 x2 (-)
		    (fun x1 x2->[T.Sub(ldest,x1,x2)]@(filter stacksize env ldest l []))
		    
    | S.BlockCreate, [ x1 ] ->
       
       let ldest = (ldest_of l) in
       (load_rvalue stacksize env x1 ldest
		    (fun x -> [T.Move(MipsArch.A(0),x)] ))@
	 allocate_stack_frame_fun @
	   [ T.Jal (T.Label "block_create" )]
	   @free_stack_frame_fun@
	     [T.Move (ldest,MipsArch.V(0))]@(filter stacksize env ldest l [])
					      
    | S.BlockGet,[ x1;  x2 ] ->
       let labl = (T.Label "block_get" ) in
       let ldest = (ldest_of l) in
       (load_rvalue stacksize env x1 ldest
		    (fun x ->
		     [T.Move(MipsArch.A(0),x)] ))@
	 (load_rvalue stacksize env x2 ldest
		      (fun x ->
		       [T.Move(MipsArch.A(1),x)] ))@
	   allocate_stack_frame_fun @
	     [ T.Jal labl]
	     @free_stack_frame_fun@
	       (**On recupère la valeur de retour *)
	       [T.Move (ldest,MipsArch.V(0))]@(filter stacksize env ldest l [])
						
						
    | S.BlockSet,[ x1; x2; x3 ] ->
       begin 
	 let labl = (T.Label "block_set" ) in
	 let ldest = (ldest_of l) in
	 (load_rvalue stacksize env x1 ldest
		      (fun x ->
		       [T.Move(MipsArch.A(0),x)] ))@
	   (load_rvalue stacksize env x2 ldest
			(fun x ->
			 [T.Move(MipsArch.A(1),x)] ))@
	     (load_rvalue stacksize env x3 ldest
			  (fun x ->
			   [T.Move(MipsArch.A(2),x)] ))@
	       allocate_stack_frame_fun @ [T.Jal labl]@free_stack_frame_fun @
		 
		 (**On recupère la valeur de retour *)
		 [T.Move (ldest,MipsArch.V(0))]@(filter stacksize env ldest l [])
       end
	 
    | S.Bool b, xs ->
       condition stacksize env b xs (S.Label "") 2 l
    | _ -> failwith "impossible"
		    
  (**and call stacksize env f rs =
  failwith "Student! This is your job!"**)

  and load_immediate r i =
    [T.Li( r, (T.Literal (Int16.of_int i)) )] 

  (** [tmp1] and [tmp2] have been reserved for this pass. *)
  and tmp1 = MipsArch.tmp1
  and tmp2 = MipsArch.tmp2

  (** [register s] turns a RetrolixHardware register name into a
      register of the target architecture. *)
  and register (S.RId s) =
    MipsArch.register_of_string s
				
  (** [mk_operation stacksize env rdest r1 r2 semantics make] compiles
      the application of an operation of two rvalues [r1] and [r2] whose
      result is stored in [rdest].

      If the two rvalues are immediate literals, [semantics] is applied
      to directly produce the result.

      Otherwise, [make] is used to emit the assembler instruction corresponding
      to the operation. [load_rvalue] is used to determine if the [rvalues] must
      be first loaded in temporary registers [tmp1] and [tmp2].
   *)

  and of_lit = function
    | S.LInt(a) ->
       (Int32.to_int a)
    | S.LBool b ->
       if b then 0
       else 1
    | S.LChar c -> Char.code c
    | _ -> assert false
		  
  and mk_operation stacksize env rdest r1 r2 semantics make =
    S.(
      match r1, r2 with
      | `Immediate i, `Immediate j ->
	 load_immediate rdest (semantics (of_lit i) (of_lit j))

      | r1, r2 ->
	 load_rvalue stacksize env r1 tmp1
		     (fun r1 ->
		      load_rvalue stacksize env r2 tmp2
				  (fun r2 ->
				   make r1 r2
				  )
		     )
    )
     

  (**and lit_of a =T.Literal (Int16.of_int a)**)
  (** [load_rvalue stacksize env rvalue rdest f] inspects [rvalue]
      to determine if it must be loaded into [rdest] before the
      emission of the instruction described by [f]. *)
  and load_rvalue stacksize env rvalue rdest f =
    match rvalue with
    | `Register r ->
       f (register r)
    | `Variable x ->
       load_variable stacksize env rdest x
       @ f rdest
    | `Immediate (S.LInt i) ->
       load_immediate rdest (Int32.to_int i) @ f rdest
    | `Immediate (S.LChar e) ->
       let codeOfe = Char.code e in 
       load_immediate rdest (codeOfe) @ f rdest
    | `Immediate (S.LBool b) ->
       if b then 
	 load_immediate rdest ( 1) @ f rdest
       else
	 load_immediate rdest (0) @ f rdest
    (*| `Immediate (S.LFun (S.FId fl)) when Options.get_gcc () ->
        T.([
          Lui (rdest, LabelAddressHi (Label fl));
          Addiu (rdest, rdest, LabelAddressLow (Label fl))
            ]) @ f rdest*)
    | `Immediate (S.LFun (S.FId fl)) -> 
       T.(La (rdest, LabelAddress (Label fl)) ) :: f rdest
    | _ -> assert false
		  
  (** [variable_address stacksize env x] returns the address
      of the variable [x]. If [x] is local, this address is
      located inside the stack frame. If [x] is global, this
      address is represented by a label. *)
  and variable_address stacksize env ((S.Id s) as x) =
    try
      let i = List.assoc x env in
      T.RegisterOffsetAddress(MipsArch.Sp, (T.Literal (Int16.of_int ((stacksize- (i*4))-stacksize ) ) ))
    with Not_found ->(
      T.LabelAddress (T.Label (global_variable_label s) )
    )
		       

  (** [load_variable stacksize env r x] emits the instructions
      to load a variable [x] in register [r]. *)
  and load_variable stacksize env r x =
    match variable_address stacksize env x with
    (**| T.LabelAddress l when Options.get_gcc () ->
       T.([
	     Lui (r, LabelAddressHi l);
	     Addiu (r, r, LabelAddressLow l);
	     Lw (r, RegisterAddress r)
	   ])**)
    | addr ->       
       [T.Lw (r, addr)]

	 
  (** [store_variable stacksize env x r] emits the instructions
      to store the value of a register [r] into a variable [x]. *)
  and store_variable (stacksize : int) env x r =
    match variable_address stacksize env x with
    (**| T.LabelAddress l when Options.get_gcc () ->
       T.([
	     Lui (tmp1, LabelAddressHi l);
	     Sw (r, RegisterOffsetAddress (tmp1, LabelAddressLow l))
	   ])**)
    | addr ->
       [T.Sw (r, addr)]


  (**  reserver à de l'espace de pile pour gcc apparement il en veut 512 pour que la 
factoriel passe bon c'est le contrat .... :/**)
  and allocate_stack_frame_fun =
    [T.Addiu(MipsArch.Sp, MipsArch.Sp, T.Literal (Int16.of_int (-((1024) * 4))) )]
      
  and free_stack_frame_fun =
    [T.Addiu(MipsArch.Sp, MipsArch.Sp, T.Literal (Int16.of_int (((1024) * 4))) )]     
  (** [allocate_stack_frame locals] modifies the stack
      pointer to introduce a fresh stack frame large
      enough to store the variables [locals]. *)
  and allocate_stack_frame locals =
    let size = List.length locals - 1 in
    [T.Addiu(MipsArch.Sp, MipsArch.Sp, T.Literal (Int16.of_int (-((size) * 4))) )]

  (** [free_stack_frame size] destructs the latest
      stack frame given the [size] of this stack frame. *)
  and free_stack_frame size =
    [T.Addiu(MipsArch.Sp, MipsArch.Sp, T.Literal (Int16.of_int (((size) * 4))) )]

      
  (** [extract_global xs d] extracts a global variable definition from [d]
      and inserts it in the list [xs]. *)
  and extract_global xs = function
    | S.DValue (S.Id x, _) -> (T.Label (global_variable_label x)) :: xs
    | _ -> xs

  in
  (** [code] is the program code of [p] compiled in MIPS for GCC. *)
  let fresh_label =
    let c = ref 0 in
    fun () -> incr c; T.Label ("_l" ^ string_of_int !c) in
  

  
  let rec constructdval = function
    | [] -> []
    | a::q  ->
       begin
	 match a with 
	 | S.DValue(id,(local,exp)) ->
	    (*Printf.printf " %d " (List.length local);*)
	    let stacksize = (List.length local )-1  in
	    let stackframe = allocate_stack_frame local in
	    let free = free_stack_frame stacksize in
	    let save = block ((stacksize*4)) local exp in
	    [T.({value = stackframe ; label = T.Label((fun (S.Id i) -> i)id )   })]@
	      save@
		[T.({value = free ; label = fresh_label ()})]@ (constructdval q)
								 
	 | _ -> []@ (constructdval q)
		      
       end  in
  let rec constructdvfun = function
    | [] -> []
    | a::q ->
       begin
	 match a with 
	 | S.DFunction(f,idl,(local,exp) ) ->
	    let locals = local@idl in
	    let stacksize = (List.length locals )-1  in
	    let stackframe = allocate_stack_frame locals in
	    let free = free_stack_frame stacksize in
	    let save = block ((stacksize*4)) locals exp in
	    
	    [T.({value = stackframe; label =T.Label ((fun (S.FId fy) -> fy)f )})]@
	      save@
		[T.({value = free@[T.Jr MipsArch.Ra] ;
		     label = fresh_label ()})] @ (constructdvfun q)
	 | _ -> []@ (constructdvfun q)
		      
       end  in
  let rec first = function
    | [] -> assert false
    | a::q ->
       begin
	 match a with
	 | S.DValue(id,(local,exp)) ->
	    (T.Label ((fun (S.Id i) -> i)id))
	 | _ -> (first q)
       end in
  let code =
    let p1 = constructdval p in
    let p2 = constructdvfun p in
    
    p2@[T.({value = [T.J (first p)] ; label = T.Label "main"})]@p1
								  
  in
  let globals = List.fold_left extract_global [] p in
  (T.({ globals;code }), ())
