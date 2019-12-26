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

  type shape = Nil | Cons of (int * shape)   
  type 'a tensor = shape ref * 'a tensordata * 'a tensordata * bool ref
  type index = S of int | All | R of (int * int)
  type indices = index array
  type _ arith = 
    | Int : (int -> int) -> 'a arith  
    | Bool : (bool -> bool) -> 'a arith
    | Float : (float -> float) -> 'a arith

  exception TypeMismatch of string
  exception Impossible
  exception NullTensor

  
(* 
  val new_t : arith -> int array -> 'a tensor
  val new_bool : int array -> bool tensor
  
  val slice : 'a tensor -> indices -> 'a tensor
  val toString : 'a tensor -> ('a -> string) -> string
  val requires_grad : 'a tensor -> bool
  val data : 'a tensor -> 'a tensordata
  val grad : 'a tensor -> 'a tensordata
  *)

  val abs : 'a tensor -> unit
  val apply : 'a arith -> 'a tensor -> unit

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

  type shape = Nil | Cons of (int * shape)   
  type 'a tensor = shape ref * 'a tensordata * 'a tensordata * bool ref
  type index = S of int | All | R of (int * int)
  type indices = index array
  type _ arith = 
    | Int : (int -> int) -> 'a arith  
    | Bool : (bool -> bool) -> 'a arith
    | Float : (float -> float) -> 'a arith

  exception TypeMismatch of string
  exception Impossible
  exception NullTensor

  let rec _apply_int f (t : int ref tensordata) : unit = 
    match t with
    | IntScalar e -> e := (f (!e))
    | IntTensor ts -> (ignore (Array.map (fun e -> _apply_int f e) ts) ; ())
    | _ -> raise Impossible

  let rec _apply_float f (t : float ref tensordata) : unit = 
    match t with
    | FloatScalar e -> e := (f (!e))
    | FloatTensor ts -> (ignore (Array.map (fun e -> _apply_float f e) ts) ; ())
    | _ -> raise Impossible

  let rec _apply_bool f (t : bool ref tensordata) : unit = 
      match t with
      | BoolScalar e -> e := (f (!e))
      | BoolTensor ts -> (ignore (Array.map (fun e -> _apply_bool f e) ts) ; ())
      | _ -> raise Impossible

  let _apply (type el) (f : 'a arith) (t : el tensordata) : unit =
    match (f, t) with
    | (_, None) -> raise NullTensor
    | (Bool f', BoolScalar e) -> e := (f' (!e))
    | (Bool f', BoolTensor e) -> _apply_bool f' (BoolTensor e)
    | (Int f', IntScalar e) -> e := (f' (!e))
    | (Int f', IntTensor e) -> _apply_int f' (IntTensor e)
    | (Float f', FloatScalar e) -> e := (f' (!e))
    | (Float f', FloatTensor e) -> _apply_float f' (FloatTensor e)
    | (Int _, FloatScalar _) -> raise (TypeMismatch "Attempted to apply int function on FloatScalar")
    | (Bool _, FloatScalar _) -> raise (TypeMismatch "Attempted to apply bool function on FloatScalar")
    | (Int _, FloatTensor _) -> raise (TypeMismatch "Attempted to apply int function on FloatTensor")
    | (Bool _, FloatTensor _) -> raise (TypeMismatch "Attempted to apply bool function on FloatTensor")
    | (Float _, IntScalar _) -> raise (TypeMismatch "Attempted to apply float function on IntScalar")
    | (Float _, IntTensor _) -> raise (TypeMismatch "Attempted to apply float function on IntTensor")
    | (Float _, BoolScalar _) -> raise (TypeMismatch "Attempted to apply float function on BoolScalar")
    | (Float _, BoolTensor _) -> raise (TypeMismatch "Attempted to apply float function on BoolTensor")

  let apply f ((shape, data, grad, requires) : 'a tensor) = _apply f data  
  
  let _abs (type el) (t : el tensordata) : unit =
    let absf v = if v > 0.0 then v else v *. (-1.0) in
    let absi v = if v > 0 then v else v * (-1) in
    let absb _ = true in
    match t with
    | BoolScalar e -> _apply (Bool absb) (BoolScalar e)
    | BoolTensor e -> _apply (Bool absb) (BoolTensor e)
    | IntScalar e -> _apply (Int absi) (IntScalar e)
    | IntTensor e -> _apply (Int absi) (IntTensor e)
    | FloatScalar e -> _apply (Float absf) (FloatScalar e)
    | FloatTensor e -> _apply (Float absf) (FloatTensor e)
  
  let abs ((shape, data, grad, requires) : 'a tensor) = _abs data
  
  (*
  let _new_bool (s : shape) = match (t,s) with
    | Nil -> BoolScalar (ref false)
    | Cons (e, Nil) -> BoolTensor (Array.init e (fun i -> ref false))
    | 
    | _ -> raise Impossible
  *)
  end 