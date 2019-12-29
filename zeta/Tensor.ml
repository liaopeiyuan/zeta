module type Tensor =
  sig

  type _ tensordata = 
    | IntScalar : int ref -> int ref tensordata
    | FloatScalar : float ref -> float ref tensordata
    | BoolScalar : bool ref -> bool ref tensordata
    | IntTensor : int ref tensordata array -> int ref tensordata
    | FloatTensor : float ref tensordata array -> float ref tensordata
    | BoolTensor : bool ref tensordata array  -> bool ref tensordata
    | None

  type shape = int array
  type 'a tensor = (shape * 'a ref tensordata * 'a ref tensordata * bool) ref
  type index = int array

  type op = 
    | IntOp : (int -> int) -> op  
    | BoolOp : (bool -> bool) -> op
    | FloatOp : (float -> float) -> op
  type predicate = 
    | IntP : (int -> bool) -> predicate  
    | BoolP : (bool -> bool) -> predicate
    | FloatP : (float -> bool) -> predicate

  exception TypeMismatch of string
  exception TensorInvariantViolated
  exception NullTensor
  exception ShapeMismatch of string

  val new_bool : shape -> bool -> bool tensor
  val new_int : shape -> int -> int tensor
  val new_float : shape -> float -> float tensor

  val reduce : predicate -> (bool * bool -> bool) -> bool -> 'a tensordata -> bool 
  val any : predicate -> 'a tensor -> bool
  val all : predicate -> 'a tensor -> bool
  val apply : op -> 'a tensor -> unit
  
  val get : 'a tensor -> index -> 'a
  val set : 'a tensor -> index -> 'a -> unit

  val abs : 'a tensor -> unit
  val sigmoid : 'a tensor -> unit

  (* val broadcast : 'a tensor -> shape -> bool -> unit *)

  (* val add : 'a tensor -> 'a tensor -> 'a tensor -> unit *)

  end 


module T  =
  struct

  type _ tensordata = 
      | IntScalar : int ref -> int ref tensordata
      | FloatScalar : float ref -> float ref tensordata
      | BoolScalar : bool ref -> bool ref tensordata
      | IntTensor : int ref tensordata array -> int ref tensordata
      | FloatTensor : float ref tensordata array -> float ref tensordata
      | BoolTensor : bool ref tensordata array  -> bool ref tensordata
      | None

  type shape = int array
  type 'a tensor = (shape * 'a ref tensordata * 'a ref tensordata * bool) ref
  type index = int array
  type op = 
    | IntOp : (int -> int) -> op  
    | BoolOp : (bool -> bool) -> op
    | FloatOp : (float -> float) -> op
  type predicate = 
    | IntP : (int -> bool) -> predicate  
    | BoolP : (bool -> bool) -> predicate
    | FloatP : (float -> bool) -> predicate
  
  exception TypeMismatch of string
  exception TensorInvariantViolated
  exception NullTensor
  exception ShapeMismatch of string
  exception IndexError of string
  exception ZeroDimension

  let rec _reduce_int (f : int -> bool) (g : bool * bool -> bool) 
                      (v : bool) (t : int ref tensordata) : bool = 
    match t with
    | IntScalar e -> f (!e)
    | IntTensor ts -> Array.fold_left (fun b p -> g (b,(_reduce_int f g v p))) v ts
    | _ -> raise TensorInvariantViolated

  let rec _reduce_float (f : float -> bool) (g : bool * bool -> bool) 
                      (v : bool) (t : float ref tensordata) : bool = 
    match t with
    | FloatScalar e -> f (!e)
    | FloatTensor ts -> Array.fold_left (fun b p -> g (b,(_reduce_float f g v p))) v ts
    | _ -> raise TensorInvariantViolated

  let rec _reduce_bool (f : bool -> bool) (g : bool * bool -> bool) 
                      (v : bool) (t : bool ref tensordata) : bool = 
    match t with
    | BoolScalar e -> f (!e)
    | BoolTensor ts -> Array.fold_left (fun b p -> g (b,(_reduce_bool f g v p))) v ts
    | _ -> raise TensorInvariantViolated

  let reduce (type el) (f : predicate) (g : bool * bool -> bool) 
                        (v : bool) (t : el tensordata) : bool =
    match (f, t) with
    | (_, None) -> raise NullTensor
    | (BoolP f', BoolScalar e) -> f' (!e)
    | (BoolP f', BoolTensor e) -> _reduce_bool f' g v (BoolTensor e)
    | (IntP f', IntScalar e) -> f' (!e)
    | (IntP f', IntTensor e) -> _reduce_int f' g v (IntTensor e)
    | (FloatP f', FloatScalar e) -> f' (!e)
    | (FloatP f', FloatTensor e) -> _reduce_float f' g v (FloatTensor e)
    | (IntP _, FloatScalar _) -> raise (TypeMismatch "Attempted to apply int predicate on FloatScalar")
    | (IntP _, BoolScalar _) -> raise (TypeMismatch "Attempted to apply int predicate on BoolScalar")
    | (BoolP _, FloatScalar _) -> raise (TypeMismatch "Attempted to apply bool predicate on FloatScalar")
    | (BoolP _, IntScalar _) -> raise (TypeMismatch "Attempted to apply bool predicate on IntScalar")
    | (IntP _, FloatTensor _) -> raise (TypeMismatch "Attempted to apply int predicate on FloatTensor")
    | (IntP _, BoolTensor _) -> raise (TypeMismatch "Attempted to apply int predicate on BoolTensor")
    | (BoolP _, FloatTensor _) -> raise (TypeMismatch "Attempted to apply bool predicate on FloatTensor")
    | (BoolP _, IntTensor _) -> raise (TypeMismatch "Attempted to apply bool predicate on IntTensor")
    | (FloatP _, IntScalar _) -> raise (TypeMismatch "Attempted to apply float predicate on IntScalar")
    | (FloatP _, IntTensor _) -> raise (TypeMismatch "Attempted to apply float predicate on IntTensor")
    | (FloatP _, BoolScalar _) -> raise (TypeMismatch "Attempted to apply float predicate on BoolScalar")
    | (FloatP _, BoolTensor _) -> raise (TypeMismatch "Attempted to apply float predicate on BoolTensor")

  let all f (t : 'a tensor) = let (shape, data, grad, requires) = !t in reduce f (fun (x,y) -> x && y) true data
  let any f (t : 'a tensor) = let (shape, data, grad, requires) = !t in reduce f (fun (x,y) -> x || y) false data

  let rec _apply_int f (t : int ref tensordata) : unit = 
    match t with
    | IntScalar e -> e := (f (!e))
    | IntTensor ts -> (ignore (Array.map (fun e -> _apply_int f e) ts) ; ())
    | _ -> raise TensorInvariantViolated

  let rec _apply_float f (t : float ref tensordata) : unit = 
    match t with
    | FloatScalar e -> e := (f (!e))
    | FloatTensor ts -> (ignore (Array.map (fun e -> _apply_float f e) ts) ; ())
    | _ -> raise TensorInvariantViolated

  let rec _apply_bool f (t : bool ref tensordata) : unit = 
    match t with
    | BoolScalar e -> e := (f (!e))
    | BoolTensor ts -> (ignore (Array.map (fun e -> _apply_bool f e) ts) ; ())
    | _ -> raise TensorInvariantViolated

  let _apply (type el) (f : op) (t : el tensordata) : unit =
    match (f, t) with
    | (_, None) -> raise NullTensor
    | (BoolOp f', BoolScalar e) -> e := (f' (!e))
    | (BoolOp f', BoolTensor e) -> _apply_bool f' (BoolTensor e)
    | (IntOp f', IntScalar e) -> e := (f' (!e))
    | (IntOp f', IntTensor e) -> _apply_int f' (IntTensor e)
    | (FloatOp f', FloatScalar e) -> e := (f' (!e))
    | (FloatOp f', FloatTensor e) -> _apply_float f' (FloatTensor e)
    | (IntOp _, FloatScalar _) -> raise (TypeMismatch "Attempted to apply int function on FloatScalar")
    | (IntOp _, BoolScalar _) -> raise (TypeMismatch "Attempted to apply int function on BoolScalar")
    | (BoolOp _, FloatScalar _) -> raise (TypeMismatch "Attempted to apply bool function on FloatScalar")
    | (BoolOp _, IntScalar _) -> raise (TypeMismatch "Attempted to apply bool function on IntScalar")
    | (IntOp _, FloatTensor _) -> raise (TypeMismatch "Attempted to apply int function on FloatTensor")
    | (IntOp _, BoolTensor _) -> raise (TypeMismatch "Attempted to apply int function on BoolTensor")
    | (BoolOp _, FloatTensor _) -> raise (TypeMismatch "Attempted to apply bool function on FloatTensor")
    | (BoolOp _, IntTensor _) -> raise (TypeMismatch "Attempted to apply bool function on IntTensor")
    | (FloatOp _, IntScalar _) -> raise (TypeMismatch "Attempted to apply float function on IntScalar")
    | (FloatOp _, IntTensor _) -> raise (TypeMismatch "Attempted to apply float function on IntTensor")
    | (FloatOp _, BoolScalar _) -> raise (TypeMismatch "Attempted to apply float function on BoolScalar")
    | (FloatOp _, BoolTensor _) -> raise (TypeMismatch "Attempted to apply float function on BoolTensor")

  let apply f (t : 'a tensor) = let (shape, data, grad, requires) = !t in _apply f data
  
  let _abs (type el) (t : el tensordata) : unit =
    let absf v = if v > 0.0 then v else v *. (-1.0) in
    let absi v = if v > 0 then v else v * (-1) in
    let absb _ = true in
    match t with
    | BoolScalar e -> _apply (BoolOp absb) (BoolScalar e)
    | BoolTensor e -> _apply (BoolOp absb) (BoolTensor e)
    | IntScalar e -> _apply (IntOp absi) (IntScalar e)
    | IntTensor e -> _apply (IntOp absi) (IntTensor e)
    | FloatScalar e -> _apply (FloatOp absf) (FloatScalar e)
    | FloatTensor e -> _apply (FloatOp absf) (FloatTensor e)
    | None -> raise NullTensor
  
  let abs (t : 'a tensor) = let (shape, data, grad, requires) = !t in _abs data

  let sigmoid (t : 'a tensor) = apply (FloatOp (fun x -> Float.exp(x) /. (Float.exp(x) +. 1.0))) t
  
  let _check_valid_shape shape =
    let len = Array.length shape in
    if (Array.fold_left (fun x y -> x || y) false (Array.init len (fun i -> (Array.get shape i)<0)) ) then raise (IndexError "Negative size along one of the dimensions")
    else if (Array.fold_left (fun x y -> x || y) false (Array.init len (fun i -> (Array.get shape i)=0)) )
    then (Printf.printf "Warning : one of the dimensions is zero. \n"; raise ZeroDimension)
    else ()

    
  let _copy (type el) (e : el ref tensordata) (b : bool) : el ref tensordata = 
    let rec _copy_bool (t : bool ref tensordata)=
      match t with
      | BoolScalar r ->  BoolScalar (ref (!r))
      | BoolTensor r -> BoolTensor (Array.map (fun i -> _copy_bool i) r)
      | _ -> raise TensorInvariantViolated 
    in
    let rec _copy_int (t : int ref tensordata) =
      match t with
      | IntScalar r -> IntScalar (ref (!r))
      | IntTensor r -> IntTensor (Array.map (fun i -> _copy_int i) r)
      | _ -> raise TensorInvariantViolated 
    in
    let rec _copy_float (t : float ref tensordata)=
      match t with
      | FloatScalar r -> FloatScalar (ref (!r))
      | FloatTensor r -> FloatTensor (Array.map (fun i -> _copy_float i) r)
      | _ -> raise TensorInvariantViolated 
    in
    if b then match e with
    | IntScalar r -> IntScalar (ref (!r))
    | FloatScalar r -> FloatScalar (ref (!r))
    | BoolScalar r -> BoolScalar (ref (!r))
    | BoolTensor r -> BoolTensor (Array.map (fun i -> _copy_bool i) r)
    | FloatTensor r -> FloatTensor (Array.map (fun i -> _copy_float i) r)
    | IntTensor r -> IntTensor (Array.map (fun i -> _copy_int i) r)
    | None -> raise NullTensor
     else e
    
  let copy t = let (shape, data, grad, requires) = !t in
    (shape, _copy(data), _copy(grad), requires)

  let rec _new_bool (s : int list) v b = match s with
    | [] -> _copy v b
    | [e] -> BoolTensor (Array.init e (fun i -> _copy v b))
    | e::s' -> BoolTensor (Array.init e (fun i -> _new_bool s' v b))

  let new_bool (s : shape) v = 
    let s' = Array.to_list s in
    let v' = BoolScalar (ref v) in
    try (_check_valid_shape s; (ref (s, _new_bool s' v' true, None, false) : bool tensor))
    with ZeroDimension -> (ref (s, BoolTensor [||], None, false))

  let rec _new_int (s : int list) v b = match s with
    | [] -> _copy v b
    | [e] -> IntTensor (Array.init e (fun i -> _copy v b))
    | e::s' -> IntTensor (Array.init e (fun i -> _new_int s' v b))

  let new_int (s : shape) v = 
    let s' = Array.to_list s in
    let v' = IntScalar (ref v) in
    try (_check_valid_shape s; (ref (s, _new_int s' v' true, None, false) : int tensor))
    with ZeroDimension -> (ref (s, IntTensor [||], None, false))


  let rec _new_float (s : int list) v b = match s with
    | [] -> _copy v b
    | [e] -> FloatTensor (Array.init e (fun i -> _copy v b))
    | e::s' -> FloatTensor (Array.init e (fun i -> _new_float s' v b))

  let new_float (s : shape) v = 
    let s' = Array.to_list s in
    let v' = FloatScalar (ref v) in
    try (_check_valid_shape s; (ref (s, _new_float s' v' true, None, false) : float tensor))
    with ZeroDimension -> (ref (s, FloatTensor [||], None, false))


  let rec _new_t (type el) (s : int list) (v : el ref tensordata) b : el ref tensordata = 
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
    | (_, None) -> raise NullTensor

  let new_t (type el) (s : shape) (t : el tensor) b = 
    let s' = (Array.to_list s) in
    let (shape, data, grad, requires) = !t in
    let news = Array.of_list( List.append s' (Array.to_list shape) ) in
    let newgrad = try (_new_t s' grad b) with NullTensor -> None in 
    try (_check_valid_shape s; (ref (news, _new_t s' data b, newgrad , requires)))
    with ZeroDimension -> match data with
      | IntScalar _ -> (ref (s, IntTensor [||], IntTensor [||], requires))
      | IntTensor _ -> (ref (s, IntTensor [||], IntTensor [||], requires))
      | FloatScalar _ -> (ref (s, FloatTensor [||], FloatTensor [||], requires))
      | FloatTensor _ -> (ref (s, FloatTensor [||], FloatTensor [||], requires))
      | BoolScalar _ -> (ref (s, BoolTensor [||], BoolTensor [||], requires))
      | BoolTensor _ -> (ref (s, BoolTensor [||], BoolTensor [||], requires))

  let rec _getset_float (t : 'a tensordata) idx f = 
    match (t, idx) with
    | (FloatScalar r, []) -> f r
    | (FloatTensor r, e::s') -> _getset_float (Array.get r e) s' f
    | _ -> raise TensorInvariantViolated

  let rec _getset_int (t : 'a tensordata) idx f = 
    match (t, idx) with
    | (IntScalar r, []) -> f r
    | (IntTensor r, e::s') -> _getset_int (Array.get r e) s' f
    | _ -> raise TensorInvariantViolated

  let rec _getset_bool (t : 'a tensordata) idx f = 
    match (t, idx) with
    | (BoolScalar r, []) -> f r
    | (BoolTensor r, e::s') -> _getset_bool (Array.get r e) s' f
    | _ -> raise TensorInvariantViolated

  let _getset (type l) (t : l ref tensordata) idx (f : l ref -> 'a) = 
    match (t, idx) with
    | (FloatScalar r, []) -> f r
    | (FloatTensor r, e::s') -> _getset_float (FloatTensor r) (e::s') f
    | (IntScalar r, []) -> f r
    | (IntTensor r, e::s') -> _getset_int (IntTensor r) (e::s') f
    | (BoolScalar r, []) -> f r
    | (BoolTensor r, e::s') -> _getset_bool (BoolTensor r) (e::s') f
    | _ -> raise TensorInvariantViolated
    
  let _check_valid_idx (data, shape, idx) =
    match data with | None -> raise NullTensor | _ -> 
    let len1 = Array.length shape in
    let len2 = Array.length idx in
    if (len1) != (len2) then raise (IndexError (("Expected index of length"^(string_of_int len1))^("; Got "^(string_of_int len2)) ) )
    else if idx < Array.init len1 (fun x -> 0) then raise (IndexError "Negative indexing not supported")
    else if not (Array.fold_left (fun x y -> x && y) true (Array.init len1 (fun i -> (Array.get idx i) < (Array.get shape i))) )
    then raise (IndexError "Array index out of bound")
    else ()

  let set (t : 'a tensor) idx e = let (shape, data, grad, requires) = !t in
    (_check_valid_idx (data, shape, idx) ; _getset data (Array.to_list idx) (fun x -> x := e))

  let get (t : 'a tensor) idx = let (shape, data, grad, requires) = !t in
    (_check_valid_idx (data, shape, idx) ; _getset data (Array.to_list idx) (fun x -> !x))
  
  (* The lists are reversed for recursion *)
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

  (* let _broadcast_int t source lead trail copy = *)
  let rec _map_int (t : int ref tensordata) source target copy = 
    match (t, source, target) with
    | (IntScalar r, [], []) -> if copy then IntScalar (ref (!r)) else IntScalar r
    | (IntTensor r, e::e', d::d') -> 
        if e = d then 
          IntTensor (Array.map (fun i -> _map_int i e' d' copy) r)
        else
          IntTensor (Array.init d (fun i -> _map_int (IntTensor r) e' d' copy))
    | (IntTensor r, [], []) -> raise TensorInvariantViolated

  
  (*  let broadcast t s copy = let (source, data, grad, requires) = !t in
    let (lead_dim, trail_dim) = _check_broadcastable ) in
    _broadcast t source lead trail copy
  *)
  end 