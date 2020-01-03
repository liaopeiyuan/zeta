module Tensor =
  struct

  type op = 
    | IntOp : (int -> int) -> op  
    | BoolOp : (bool -> bool) -> op
    | FloatOp : (float -> float) -> op

  type predicate = 
    | IntP : (int -> bool) -> predicate  
    | BoolP : (bool -> bool) -> predicate
    | FloatP : (float -> bool) -> predicate

  type 'a tensordata = 
      | IntScalar : int ref -> int tensordata
      | FloatScalar : float ref -> float tensordata
      | BoolScalar : bool ref -> bool tensordata
      | IntTensor : int tensordata array -> int tensordata
      | FloatTensor : float tensordata array -> float tensordata
      | BoolTensor : bool tensordata array  -> bool tensordata
      
  type shape = int array
  type index = int array

  (* Next Layer *)
  (* Gradient : RetainRequire for freshly constructed graphs and tensors that don't require gradient
                 and Grad for backpropped graphs
                bool used to encode retrain_grad option
   *)
  (* Directed Acyclic Graph (DAG) for tensor operations: 
      Null for "isolated" tensors that do not belong to any graph
      Graph for tensors as nodes in the graph - 
          gradient, node it's connected to with associated operations and node that connects to it
  *)
  type 'a grad_fn = End | Fn of ('a tensor * op) array
  and 'a gradient = Retain of bool | Grad of (bool * 'a tensordata)
  and 'a parent = 'a tensor array
  and 'a node = LeafNoGrad | LeafGrad of 'a gradient | Node of ('a parent * 'a gradient)
  and 'a directed_acyclic_graph = Null | Graph of ('a grad_fn * 'a node)
  (* shape describes the dimensions of the data *)
  and 'a tensor = (shape * 'a tensordata * 'a directed_acyclic_graph ) ref  
  
  exception TypeMismatch of string
  exception TensorInvariantViolated
  exception ShapeMismatch of string
  exception IndexError of string
  exception ZeroDimension
  exception AutogradError of string

  let is_leaf (t : 'a tensor) : bool = let (shape, data, dag) = !t in 
    match dag with
    | Null -> true
    | Graph (_, LeafNoGrad) -> true
    | Graph (_, LeafGrad _) -> true
    | Graph (_, Node _) -> false

  let requries_grad (t : 'a tensor) (b : bool) : unit = let (shape, data, dag) = !t in 
    match (b, dag) with
    | (true, Null) -> t := (shape, data, Graph (End, LeafGrad (Retain false)))
    | (false, Null) -> Printf.printf "Warning : isolated leaf tensors does not require gradient. \n"
    | (_, Graph (_, Node _)) -> raise (AutogradError "you can only change requires_grad flags of leaf tensors.")
    | (true, Graph (_, LeafGrad _)) -> Printf.printf "Warning : tensor requires gradient already. \n"
    | (false, Graph (a, LeafGrad _)) -> t := (shape, data, Graph (a, LeafNoGrad))
    | (true, Graph (a, LeafNoGrad)) -> t := (shape, data, Graph (a, LeafGrad (Retain false)))
    | (false, Graph (a, LeafNoGrad)) -> Printf.printf "Warning : leaf tensors does not require gradient already. \n"
   
  let retain_grad (t : 'a tensor) (b : bool) : unit = let (shape, data, dag) = !t in 
    match dag with
    | Null -> raise (AutogradError "tensor does not require gradient.")
    | Graph (a, Node (p, Retain _)) -> t := (shape, data, Graph (a, Node (p, Retain b)))
    | Graph (a, Node (p, Grad (_, d))) -> t := (shape, data, Graph (a, Node (p, Grad (b, d))) )
    | Graph (a, LeafNoGrad ) -> raise (AutogradError "leaf tensor does not require gradient.")
    | Graph (a, LeafGrad (Retain _) ) -> t := (shape, data, Graph (a, LeafGrad (Retain b) ) )
    | Graph (a, LeafGrad (Grad (_,d)) ) -> t := (shape, data, Graph (a, LeafGrad (Grad (b,d)) ) )
  
  let detach (t : 'a tensor) : 'a tensor = let (shape, data, dag) = !t in ref (shape, data, Null)

  let _check_valid_shape shape =
    let len = Array.length shape in
    if (Array.fold_left (fun x y -> x || y) false (Array.init len (fun i -> (Array.get shape i)<0)) ) then raise (IndexError "Negative size along one of the dimensions")
    else if (Array.fold_left (fun x y -> x || y) false (Array.init len (fun i -> (Array.get shape i)=0)) )
    then (Printf.printf "Warning : one of the dimensions is zero. \n"; raise ZeroDimension)
    else ()
 
  let rec _copy : 'a. 'a tensordata -> bool -> 'a tensordata =
    fun (type el) (e : el tensordata) (b : bool) : el tensordata ->
      if b then match e with
      | IntScalar r -> IntScalar (ref (!r))
      | FloatScalar r -> FloatScalar (ref (!r))
      | BoolScalar r -> BoolScalar (ref (!r))
      | BoolTensor r -> BoolTensor (Array.map (fun i -> _copy i b) r)
      | FloatTensor r -> FloatTensor (Array.map (fun i -> _copy i b) r)
      | IntTensor r -> IntTensor (Array.map (fun i -> _copy i b) r)
      else e
    
  let copy t = let (shape, data, dag) = !t in
    ref (shape, _copy data true, dag)

  let rec _new_bool (s : int list) v b = match s with
    | [] -> _copy v b
    | [e] -> BoolTensor (Array.init e (fun i -> _copy v b))
    | e::s' -> BoolTensor (Array.init e (fun i -> _new_bool s' v b))

  let rec _new_int (s : int list) v b = match s with
    | [] -> _copy v b
    | [e] -> IntTensor (Array.init e (fun i -> _copy v b))
    | e::s' -> IntTensor (Array.init e (fun i -> _new_int s' v b))
  
  let rec _new_float (s : int list) v b = match s with
    | [] -> _copy v b
    | [e] -> FloatTensor (Array.init e (fun i -> _copy v b))
    | e::s' -> FloatTensor (Array.init e (fun i -> _new_float s' v b))
  
  let new_bool (s : shape) v = 
    let s' = Array.to_list s in
    let v' = BoolScalar (ref v) in
    try (_check_valid_shape s; (ref (s, _new_bool s' v' true, Null) : bool tensor))
    with ZeroDimension -> (ref (s, BoolTensor [||], Null))

  let new_int (s : shape) v = 
    let s' = Array.to_list s in
    let v' = IntScalar (ref v) in
    try (_check_valid_shape s; (ref (s, _new_int s' v' true, Null) : int tensor))
    with ZeroDimension -> (ref (s, IntTensor [||], Null))

  let new_float (s : shape) v = 
    let s' = Array.to_list s in
    let v' = FloatScalar (ref v) in
    try (_check_valid_shape s; (ref (s, _new_float s' v' true, Null) : float tensor))
    with ZeroDimension -> (ref (s, FloatTensor [||], Null))

  let rec _reduce : 'a. predicate -> (bool * bool -> bool) -> bool -> 'a tensordata -> bool =
    fun (type el) f g v (t : el tensordata) : bool ->
      match (f, t) with
      | (BoolP f', BoolScalar e) -> f' (!e)
      | (BoolP f', BoolTensor e) -> Array.fold_left (fun b p -> g (b,(_reduce f g v p))) v e
      | (IntP f', IntScalar e) -> f' (!e)
      | (IntP f', IntTensor e) -> Array.fold_left (fun b p -> g (b,(_reduce f g v p))) v e
      | (FloatP f', FloatScalar e) -> f' (!e)
      | (FloatP f', FloatTensor e) -> Array.fold_left (fun b p -> g (b,(_reduce f g v p))) v e
      | (_, _) -> raise (TypeMismatch "You can only apply predicate and tensor of the same type")

  let reduce f g v (t : 'a tensor) = let (shape, data, dag) = !t in _reduce f g v data
  let all f (t : 'a tensor) = let (shape, data, dag) = !t in _reduce f (fun (x,y) -> x && y) true data
  let any f (t : 'a tensor) = let (shape, data, dag) = !t in _reduce f (fun (x,y) -> x || y) false data

  let rec _elem_apply : 'a. op -> 'a tensordata -> 'a tensordata = 
    fun (type el) (f : op) (t : el tensordata) : el tensordata ->
      match (f, t) with
      | (BoolOp f', BoolScalar e) -> BoolScalar (ref (f' (!e)))
      | (BoolOp f', BoolTensor e) -> BoolTensor (Array.map (fun i -> _elem_apply f i) e)
      | (IntOp f', IntScalar e) -> IntScalar (ref (f' (!e)))
      | (IntOp f', IntTensor e) ->  IntTensor (Array.map (fun i -> _elem_apply f i) e)
      | (FloatOp f', FloatScalar e) -> FloatScalar (ref (f' (!e)))
      | (FloatOp f', FloatTensor e) ->  FloatTensor (Array.map (fun i -> _elem_apply f i) e)
      | (_, _) -> raise (TypeMismatch "You can only apply op and tensor of the same type")

  let elem_apply f (t : 'a tensor) = let (shape, data, dag) = !t in 
    let newd = _elem_apply f data in
    match dag with
    | Null -> ref (shape, newd , Null)
    | Graph (End, x) -> let newt = ref (shape, newd, Graph (End, Node ([|t|], Retain false)) ) in 
                        ( Printf.printf "Warning : you can only back-propagate with float tensors. \n" ;
                          t := (shape, data, Graph (Fn [|(newt, f)|], x )) ; 
                          newt 
                        )
    | Graph (Fn l, x) -> let newt = ref (shape, newd, Graph (End, Node ([|t|], Retain false)) ) in 
                        ( Printf.printf "Warning : you can only back-propagate with float tensors. \n" ;
                          t := (shape, data, Graph (Fn (Array.append [|(newt, f)|] l), x )) ; 
                          newt 
                        )

  let sigmoid (t : 'a tensor) = elem_apply (FloatOp (fun x -> Float.exp(x) /. (Float.exp(x) +. 1.0))) t
  
  let _abs (type el) (t : el tensordata) : el tensordata =
    let absf v = if v > 0.0 then v else v *. (-1.0) in
    let absi v = if v > 0 then v else v * (-1) in
    let absb _ = true in
    match t with
    | BoolScalar e -> _elem_apply (BoolOp absb) (BoolScalar e)
    | BoolTensor e -> _elem_apply (BoolOp absb) (BoolTensor e)
    | IntScalar e -> _elem_apply (IntOp absi) (IntScalar e)
    | IntTensor e -> _elem_apply (IntOp absi) (IntTensor e)
    | FloatScalar e -> _elem_apply (FloatOp absf) (FloatScalar e)
    | FloatTensor e -> _elem_apply (FloatOp absf) (FloatTensor e)
  
  let abs (t : 'a tensor) = 
    let (shape, data, dag) = !t in 
    let newd = _abs data in
    match dag with
    | Null -> ref (shape, newd , Null)
    | Graph (End, x) -> let newt = ref (shape, newd, Graph (End, Node ([|t|], Retain false)) ) in 
                        ( Printf.printf "Warning : you can only back-propagate with float tensors. \n" ;
                          t := (shape, data, Graph (Fn [|(newt, FloatOp (fun x -> x))|], x )) ; 
                          newt 
                        )
    | Graph (Fn l, x) -> let newt = ref (shape, newd, Graph (End, Node ([|t|], Retain false)) ) in 
                        ( Printf.printf "Warning : you can only back-propagate with float tensors. \n" ;
                          t := (shape, data, Graph (Fn (Array.append [|(newt, FloatOp (fun x -> x))|] l), x )) ; 
                          newt 
                        )

  let rec _new_t (type el) (s : int list) (v : el tensordata) b : el tensordata = 
    let f t b = if b then ref (!t) else t in
    match (s,v) with
    | ([], IntScalar t) -> IntScalar (f t b)
    | (e::s', IntScalar t) -> _new_int (e::s') (IntScalar t) b
    | ([], IntTensor t) -> _copy (IntTensor t) b
    | (e::s', IntTensor t) -> _new_int (e::s') (IntTensor t) b
    | ([], FloatTensor t) -> _copy (FloatTensor t) b
    | (e::s', FloatTensor t) -> _new_float (e::s') (FloatTensor t) b
    | ([], FloatScalar t) -> FloatScalar (f t b)
    | (e::s', FloatScalar t) -> _new_float (e::s') (FloatScalar t) b
    | ([], BoolScalar t) -> BoolScalar (f t b)
    | (e::s', BoolScalar t) -> _new_bool (e::s') (BoolScalar t) b
    | ([], BoolTensor t) -> _copy (BoolTensor t) b
    | (e::s', BoolTensor t) -> _new_bool (e::s') (BoolTensor t) b

  let new_t (type el) (s : shape) (t : el tensor) b = 
    let s' = (Array.to_list s) in
    let (shape, data, grad, grad_fn) = !t in
    let news = Array.of_list( List.append s' (Array.to_list shape) ) in
    let newgrad = (_new_t s' grad b) in 
    let newt = 
    try (_check_valid_shape s; (news, _new_t s' data b, newgrad , grad_fn))
    with ZeroDimension -> match data with
      | IntScalar _ ->  (s, IntTensor [||], IntTensor [||], grad_fn)
      | IntTensor _ ->  (s, IntTensor [||], IntTensor [||], grad_fn)
      | FloatScalar _ ->  (s, FloatTensor [||], FloatTensor [||], grad_fn)
      | FloatTensor _ ->  (s, FloatTensor [||], FloatTensor [||], grad_fn)
      | BoolScalar _ ->  (s, BoolTensor [||], BoolTensor [||], grad_fn)
      | BoolTensor _ ->  (s, BoolTensor [||], BoolTensor [||], grad_fn)
    in
    if b then ref newt else (t := newt; t)

  let rec _getset: 'a. 'a tensordata -> int list -> ('a ref -> 'b) -> 'b =
    fun (type el) (t : el tensordata) idx (f : el ref -> 'a) -> 
    match (t, idx) with
    | (FloatScalar r, []) -> f r
    | (FloatTensor r, e::s') -> _getset (Array.get r e) s' f
    | (IntScalar r, []) -> f r
    | (IntTensor r, e::s') -> _getset (Array.get r e) s' f
    | (BoolScalar r, []) -> f r
    | (BoolTensor r, e::s') -> _getset (Array.get r e) s' f
    | _ -> raise TensorInvariantViolated
    
  let _check_valid_idx (data, shape, idx) =
    let len1 = Array.length shape in
    let len2 = Array.length idx in
    if (len1) != (len2) then raise (IndexError (("Expected index of length "^(string_of_int len1))^("; Got "^(string_of_int len2)) ) )
    else if idx < Array.init len1 (fun x -> 0) then raise (IndexError "Negative indexing not supported")
    else if not (Array.fold_left (fun x y -> x && y) true (Array.init len1 (fun i -> (Array.get idx i) < (Array.get shape i))) )
    then raise (IndexError "Array index out of bound")
    else ()

  let set (t : 'a tensor) idx e = let (shape, data, grad, grad_fn) = !t in
    (_check_valid_idx (data, shape, idx) ; _getset data (Array.to_list idx) (fun x -> x := e))

  let get (t : 'a tensor) idx = let (shape, data, grad, grad_fn) = !t in
    (_check_valid_idx (data, shape, idx) ; _getset data (Array.to_list idx) (fun x -> !x))

  (* dangerous *)
  let _set t idx e = _getset t (Array.to_list idx) (fun x -> x := e)
  
  let _check_broadcastable s d =
    let (source, destination) = ((List.rev (Array.to_list s)), (List.rev (Array.to_list d))) in
    let rec _check_broadcastable' source destination = 
      match (source, destination) with
        | ([], d) -> (destination, [])
        | (_ :: _,[]) -> raise (ShapeMismatch "source array has more dimensions than desired shape")
        | (s :: s', d :: d') -> 
          if s != d && s != 1 then raise (ShapeMismatch "one of the trailing dimensions don't agree")
          else let (lead, trail) = _check_broadcastable' s' d' in (lead, d::trail) in
    let (s', d') = _check_broadcastable' source destination in
      (List.rev s', List.rev d')

  let rec _map: 'a.  'a tensordata -> int list -> int list -> bool -> 'a tensordata =
    fun (type el) (t : el tensordata) source target copy : el tensordata -> 
    match (t, source, target) with
    | (IntScalar r, [], []) -> if copy then IntScalar (ref (!r)) else IntScalar r
    | (IntTensor r, e::e', d::d') -> 
        if e = d then 
          IntTensor (Array.map (fun i -> _map i e' d' copy) r)
        else
          IntTensor (Array.init d (fun _ -> _map (Array.get r 0) e' d' copy))
    | (IntTensor r, [], []) -> IntTensor r
    | (FloatScalar r, [], []) -> if copy then FloatScalar (ref (!r)) else FloatScalar r
    | (FloatTensor r, e::e', d::d') -> 
        if e = d then 
          FloatTensor (Array.map (fun i -> _map i e' d' copy) r)
        else
          FloatTensor (Array.init d (fun _ -> _map (Array.get r 0) e' d' copy))
    | (FloatTensor r, [], []) -> FloatTensor r
    | (BoolScalar r, [], []) -> if copy then BoolScalar (ref (!r)) else BoolScalar r
    | (BoolTensor r, e::e', d::d') -> 
        if e = d then 
          BoolTensor (Array.map (fun i -> _map i e' d' copy) r)
        else
          BoolTensor (Array.init d (fun _ -> _map (Array.get r 0) e' d' copy))
    | (BoolTensor r, [], []) -> BoolTensor r
    | _ -> raise TensorInvariantViolated
  
  
  let _broadcast (type el) (t : el tensordata) 
                 (source : int list) (lead : int list)
                 (trail : int list) (copy : bool) : el tensordata =  
    let f t b = if b then ref (!t) else t in
    match t with
    | FloatTensor r -> _new_float lead (_map (FloatTensor r) source trail copy) copy
    | BoolTensor r -> _new_bool lead (_map (BoolTensor r) source trail copy) copy
    | IntTensor r -> _new_int lead (_map (IntTensor r) source trail copy) copy
    | IntScalar r -> _new_int lead (IntScalar (f r copy)) copy
    | FloatScalar r -> _new_float lead (FloatScalar (f r copy)) copy
    | BoolScalar r -> _new_bool lead (BoolScalar (f r copy)) copy
    | _ -> raise TensorInvariantViolated

  let broadcast t destination copy = let (source, data, grad, grad_fn) = !t in
    let (lead_dim, trail_dim) = _check_broadcastable source destination in
    let newdata = _broadcast data (Array.to_list source) lead_dim trail_dim copy in
    let news = Array.of_list (lead_dim @ trail_dim) in
      ref (news, newdata, grad, grad_fn)

  let rec _elem_mul : 'a. 'a tensordata -> 'a tensordata -> 'a tensordata =
    fun (type el) (t1 : el tensordata) (t2 : el tensordata) : el tensordata ->
    match (t1, t2) with
    | (BoolScalar s, BoolScalar s') -> BoolScalar (ref (!s && !s'))
    | (BoolScalar s, BoolTensor t) -> if !s then _copy (BoolTensor t) true else _apply (BoolOp (fun _ -> false)) (BoolTensor t)
    | (BoolTensor t, BoolScalar s) -> if !s then _copy (BoolTensor t) true else _apply (BoolOp (fun _ -> false)) (BoolTensor t)
    | (IntScalar s, IntScalar s') -> IntScalar (ref (!s * !s'))
    | (IntScalar s, IntTensor t) -> _apply (IntOp (fun i -> !s * i)) (IntTensor t)
    | (IntTensor t, IntScalar s) -> _apply (IntOp (fun i -> !s * i)) (IntTensor t)
    | (FloatScalar s, FloatScalar s') -> FloatScalar (ref (!s *. !s'))
    | (FloatScalar s, FloatTensor t) -> _apply (FloatOp (fun i -> !s *. i)) (FloatTensor t)
    | (FloatTensor t, FloatScalar s) -> _apply (FloatOp (fun i -> !s *. i)) (FloatTensor t)
    | (FloatTensor t, FloatTensor t') -> FloatTensor (Array.mapi (fun i e -> _elem_mul (Array.get t i) e) t')
    | (IntTensor t, IntTensor t') -> IntTensor (Array.mapi (fun i e -> _elem_mul (Array.get t i) e) t')
    | (BoolTensor t, BoolTensor t') -> BoolTensor (Array.mapi (fun i e -> _elem_mul (Array.get t i) e) t')
    | (_, _) -> raise (TypeMismatch "you can only multiply tensors of the same kind")
  
    
  let (#*) (t1 : 'a tensor) (t2 : 'a tensor) : 'a tensor = 
    let ((s1, d1, g1, r1),(s2, d2, g2, r2)) = (!t1, !t2) in
    let max_dim s1 s2 =
      let (l1, l2) = ((List.rev (Array.to_list s1)),(List.rev (Array.to_list s2))) in
      let rec max_dim' l1 l2 = 
        match (l1, l2) with
         | ([], []) -> []
         | (x::xs, []) -> x::xs
         | ([], x::xs) -> x::xs
         | (x::xs, y::ys) -> (max x y)::(max_dim' xs ys) in
      List.rev (max_dim' l1 l2) in
    let news = Array.of_list (max_dim s1 s2) in
    match (Array.length s1, Array.length s2, s1=s2) with
        | (0, _, _) ->  ref (s2,_elem_mul d1 d2,g2,r2)
        | (_, 0, _) ->  ref (s1,_elem_mul d1 d2,g1,r1)
        | (_, _, true) -> ref (s1,_elem_mul d1 d2,g1,r1)
        | (_, _, _) -> 
          let 
          ((_,t1',g1',r1'),(_,t2',_,_)) = (!(broadcast t1 news true),!(broadcast t2 news true)) in
          ref (news,_elem_mul t1' t2',g1',r1')
     
  
    (*  
    | (IntScalar r, []) -> IntScalar (f r copy)
 | (FloatScalar r, []) -> FloatScalar (f r copy)
     | (BoolScalar r, []) -> BoolScalar (f r copy)
  
  let broadcast t s copy = let (source, data, grad, grad_fn) = !t in
    let (lead_dim, trail_dim) = _check_broadcastable ) in
    _broadcast t source lead trail copy
  *)
  end 