 (** [occurs k t] returns [true] if parameter [TParam k] appears in type [t]. *)
 let rec occurs k = function
    | TInt -> false
    | TBool -> false
    | TParam j -> k = j
    | TArrow (t1, t2) -> occurs k t1 || occurs k t2
    | TTimes (t1, t2) -> occurs k t1 || occurs k t2
    | TList t -> occurs k t
  
  (** [solve [(t1,u1); ...; (tn,un)] solves the system of equations
      [t1=u1], ..., [tn=un]. The solution is represented by a list of
      pairs [(k,t)], meaning that [TParam k] equals [t]. A type error is
       raised if there is no solution. The solution found is the most general
       one.
   *)
   let solve eq =
    let rec solve eq sbst =
      match eq with
         | [] -> sbst
             
         | (t1, t2) :: eq when t1 = t2 -> solve eq sbst
             
        | ((TParam k, t) :: eq | (t, TParam k) :: eq) when (not (occurs k t)) ->
            let ts = tsubst [(k,t)] in
              solve
                (List.map (fun (ty1,ty2) -> (ts ty1, ts ty2)) eq)
                ((k,t)::(List.map (fun (n, u) -> (n, ts u)) sbst))
             
       | (TTimes (u1,v1), TTimes (u2,v2)) :: eq
        | (TArrow (u1,v1), TArrow (u2,v2)) :: eq ->
            solve ((u1,u2)::(v1,v2)::eq) sbst
              
         | (TList t1, TList t2) :: eq -> solve ((t1,t2) :: eq) sbst
             
         | (t1,t2)::_ ->
             let u1, u2 = rename2 t1 t2 in
              type_error ("The types " ^ string_of_type u1 ^ " and " ^
                          string_of_type u2 ^ " are incompatible")
    in
      solve eq []