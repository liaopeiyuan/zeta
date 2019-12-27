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

  (* val broadcast : 'a tensor -> shape -> unit *)

  (* val add : 'a tensor -> 'a tensor -> 'a tensor -> unit *)

  end 


module T : Tensor =
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
  
  let rec _new_bool (s : int list) v = match s with
    | [] -> BoolScalar (ref v)
    | [e] -> BoolTensor (Array.init e (fun i -> BoolScalar (ref v)))
    | e::s' -> BoolTensor (Array.init e (fun i -> _new_bool s' v))

  let new_bool (s : shape) v = 
    let s' = Array.to_list s in
    (ref (s, _new_bool s' v, None, false) : bool tensor)

  let rec _new_int (s : int list) v = match s with
    | [] -> IntScalar (ref v)
    | [e] -> IntTensor (Array.init e (fun i -> IntScalar (ref v)))
    | e::s' -> IntTensor (Array.init e (fun i -> _new_int s' v))

  let new_int (s : shape) v = 
    let s' = Array.to_list s in
    (ref (s, _new_int s' v, None, false) : int tensor)

  let rec _new_float (s : int list) v = match s with
    | [] -> FloatScalar (ref v)
    | [e] -> FloatTensor (Array.init e (fun i -> FloatScalar (ref v)))
    | e::s' -> FloatTensor (Array.init e (fun i -> _new_float s' v))

  let new_float (s : shape) v = 
    let s' = Array.to_list s in
    (ref (s, _new_float s' v, None, false) : float tensor)

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

  end 