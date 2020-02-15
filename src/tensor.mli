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
type index_selection = Range of (int * int) | Index of int | All
and slice = index_selection array
type index = int array
type 'a grad_fn = End | Fn of ('a tensor * op) array
and 'a gradient = Retain of bool | Grad of (bool * 'a tensordata)
and 'a parent = 'a tensor array
and 'a node = LeafNoGrad | LeafGrad of 'a gradient | Node of ('a parent * 'a gradient)
and 'a directed_acyclic_graph = Null | Graph of ('a grad_fn * 'a node)
and 'a tensor = (shape * 'a tensordata * 'a directed_acyclic_graph ) ref
exception TypeMismatch of string
exception TensorInvariantViolated
exception ShapeMismatch of string
exception IndexError of string
exception ZeroDimension
exception AutogradError of string
val is_leaf : 'a tensor -> bool
val requires_grad : 'a tensor -> bool -> unit
val retain_grad : 'a tensor -> bool -> unit
val detach : 'a tensor -> 'a tensor
val copy : ('a * 'b tensordata * 'c) ref -> ('a * 'b tensordata * 'c) ref
val slice : slice -> 'a tensor -> 'a tensor 
val new_bool : shape -> bool -> bool tensor
val new_int : shape -> int -> int tensor
val new_float : shape -> float -> float tensor
val reduce : predicate -> (bool * bool -> bool) -> bool -> 'a tensor -> bool
val all : predicate -> 'a tensor -> bool
val any : predicate -> 'a tensor -> bool
val elem_apply : op -> 'a tensor -> (shape * 'a tensordata * 'a directed_acyclic_graph) ref
val sigmoid : 'a tensor -> (shape * 'a tensordata * 'a directed_acyclic_graph) ref
val abs : 'a tensor -> (shape * 'a tensordata * 'a directed_acyclic_graph) ref
val new_t : shape -> 'a tensor -> bool -> (shape * 'a tensordata * 'a directed_acyclic_graph) ref
val set : 'a tensor -> int array -> 'a -> unit
val get : 'a tensor -> int array -> 'a
val broadcast : (int array * 'a tensordata * 'b) ref -> int array -> bool -> (int array * 'a tensordata * 'b) ref
val (#*) : 'a tensor -> 'a tensor -> 'a tensor
