module type Tensor =
  sig

  type 'a tensordata = 
    | Int : int ref -> int ref tensordata
    | Float : float ref -> float ref tensordata
    | Bool : bool ref -> bool ref tensordata
    | IntTensor : int ref tensordata array -> int ref tensordata
    | FloatTensor : float ref tensordata array -> float ref tensordata
    | BoolTensor : bool ref tensordata array  -> bool ref tensordata
    | None

  type shape = Nil | Cons of (int * shape)   
  type 'a tensor = shape ref * 'a tensordata * 'a tensordata * bool ref
  type index = S of int | All | R of (int * int)
  type indices = index array

  exception TypeMismatch of string
  exception Impossible

  (* 
  val slice : 'a tensor -> indices -> 'a tensor
  val toString : 'a tensor -> ('a -> string) -> string
  val requires_grad : 'a tensor -> bool
  val data : 'a tensor -> 'a tensordata
  val grad : 'a tensor -> 'a tensordata
  *)

  val abs : 'a tensor -> unit
  val apply : (bool -> bool) -> (int -> int) -> (float -> float) -> 'a tensor -> unit

  end 


module T : Tensor =
  struct

  type 'a tensordata = 
      | Int : int ref -> int ref tensordata
      | Float : float ref -> float ref tensordata
      | Bool : bool ref -> bool ref tensordata
      | IntTensor : int ref tensordata array -> int ref tensordata
      | FloatTensor : float ref tensordata array -> float ref tensordata
      | BoolTensor : bool ref tensordata array  -> bool ref tensordata
      | None

  type shape = Nil | Cons of (int * shape)   
  type 'a tensor = shape ref * 'a tensordata * 'a tensordata * bool ref
  type index = S of int | All | R of (int * int)
  type indices = index array

  exception TypeMismatch of string
  exception Impossible
  exception NullTensor

  let rec _apply_int (type el) f (t : el tensordata) : unit = 
    match t with
    | Int e -> e := (f (!e))
    | IntTensor ts -> (ignore (Array.map (fun e -> _apply_int f e) ts) ; ())
    | _ -> raise Impossible

  let rec _apply_float (type el) f (t : el tensordata) : unit = 
    match t with
    | Float e -> e := (f (!e))
    | FloatTensor ts -> (ignore (Array.map (fun e -> _apply_float f e) ts) ; ())
    | _ -> raise Impossible

  let rec _apply_bool (type el) f (t : el tensordata) : unit = 
      match t with
      | Bool e -> e := (f (!e))
      | BoolTensor ts -> (ignore (Array.map (fun e -> _apply_bool f e) ts) ; ())
      | _ -> raise Impossible

  let _apply (type el) fb fi ff (t : el tensordata) : unit =
    match t with
    | Bool e -> _apply_bool fb (Bool e)
    | BoolTensor e -> _apply_bool fb (BoolTensor e)
    | Int a -> _apply_int fi (Int a)
    | IntTensor a -> _apply_int fi (IntTensor a)
    | Float a -> _apply_float ff (Float a)
    | FloatTensor a -> _apply_float ff (FloatTensor a)
    | _ -> raise NullTensor

  let _abs (type el) (t : el tensordata) : unit =
    let absf v = if v > 0.0 then v else v *. (-1.0) in
    let absi v = if v > 0 then v else v * (-1) in
    let absb _ = true in
    _apply absb absi absf t
  (* TODO: GADT for inplace operations *)

  let apply fb fi ff ((shape, data, grad, requires) : 'a tensor) = _apply fb fi ff data
  let abs ((shape, data, grad, requires) : 'a tensor) = _abs data
  end 