module type Tensor =
  sig

  type _ tensordata = 
    | IntScalar : int ref -> int ref tensordata
    | FloatScalar : float ref -> float ref tensordata
    | BoolScalar : bool ref -> bool ref tensordata
    | IntTensor : int ref tensordata array -> int ref tensordata
    | FloatTensor : float ref tensordata array -> float ref tensordata
    | BoolTensor : bool ref tensordata array  -> bool ref tensordata
    | Null
  type 'a grad_fn = Empty | Fn of 'a ref tensordata ref list
  type shape = int array
  type 'a tensor = (shape * 'a ref tensordata * 'a ref tensordata * 'a grad_fn) ref
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
