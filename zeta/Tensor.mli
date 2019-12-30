module type Tensor =
  sig

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
      | Null
      
  type shape = int array
  type index = int array

  type 'a grad_fn = 
    | Empty : 'a grad_fn 
    | Fn : ('a tensor * op) array -> 'a grad_fn
  and 'a tensor = (shape * 'a tensordata * 'a tensordata * 'a grad_fn) ref  
  
  exception TypeMismatch of string
  exception TensorInvariantViolated
  exception NullTensor
  exception ShapeMismatch of string
  exception IndexError of string
  exception ZeroDimension

  val new_bool : shape -> bool -> bool tensor
  val new_int : shape -> int -> int tensor
  val new_float : shape -> float -> float tensor
  val copy : 'a tensor -> 'a tensor
  
  val new_t : shape -> 'a tensor -> bool -> 'a tensor
  val broadcast : 'a tensor -> shape -> bool -> 'a tensor

  val reduce : predicate -> (bool * bool -> bool) -> bool -> 'a tensordata -> bool 
  val any : predicate -> 'a tensor -> bool
  val all : predicate -> 'a tensor -> bool

  val apply : op -> 'a tensor -> unit
  
  val get : 'a tensor -> index -> 'a
  val set : 'a tensor -> index -> 'a -> unit

  val abs : 'a tensor -> unit
  val sigmoid : 'a tensor -> unit

  end 
