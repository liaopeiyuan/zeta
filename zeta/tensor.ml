module Tensor :
  sig

  type 'a tensordata = 
      | Scalar : 'a -> 'a tensordata
      | Tensor : 'a tensordata array  -> 'a tensordata
      | IntTensor : int tensordata array -> int tensordata
      | FloatTensor : float tensordata array -> float tensordata

  type shape = Nil | Cons of (int * shape)   
  type 'a tensor = shape ref * 'a tensordata * 'a tensordata * bool ref
  type index = S of int | All | R of (int * int)
  type indices = index array

  val slice : 'a tensor -> indices -> 'a tensor
  val requires_grad : 'a tensor -> bool
  val data : 'a tensor -> 'a tensordata
  val grad : 'a tensor -> 'a tensordata

  val set_requires_grad : 'a tensor -> ()
  
  val _abs : 'a tensor -> ()
  (* TODO: GADT for inplace operations *)

  val _add : 'a tensor -> 'a tensor -> 'a -> ()

  end 

