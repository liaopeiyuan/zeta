module type Sequence =
  sig
    type 'a t = 'a array
    type order = Less | Equal | Greater
    type 'a ord = 'a * 'a -> order

    exception IndexOutOfRange
    exception NegativeSize

    val nth : 'a t -> int -> 'a
    val length : 'a t -> int
    val toList : 'a t -> 'a list
    val toString : ('a -> string) -> 'a t -> string
    val equal : ('a * 'a -> bool) -> 'a t * 'a t -> bool

    val empty : unit -> 'a t
    val singleton : 'a -> 'a t
    val tabulate : (int -> 'a) -> int -> 'a t
    val fromList : 'a list -> 'a t

    val rev : 'a t -> 'a t
    val append : 'a t * 'a t -> 'a t
    val flatten : 'a t t -> 'a t

    val filter : ('a -> bool) -> 'a t -> 'a t
    val map : ('a -> 'b) -> 'a t -> 'b t
    val zip : 'a t * 'b t -> ('a * 'b) t
    val zipWith : ('a * 'b -> 'c) -> 'a t * 'b t -> 'c t

    val enum : 'a t -> (int * 'a) t
    val filterIdx : (int * 'a -> bool) -> 'a t -> 'a t
    val mapIdx : (int * 'a -> 'b) -> 'a t -> 'b t
    val update : 'a t * (int * 'a) -> 'a t
    val inject : 'a t * (int * 'a) t -> 'a t

    val subseq : 'a t -> int * int -> 'a t
    val take : 'a t -> int -> 'a t
    val drop : 'a t -> int -> 'a t

    val iterate : ('b * 'a -> 'b) -> 'b -> 'a t -> 'b
    val reduce : ('a * 'a -> 'a) -> 'a -> 'a t -> 'a
    val scan : ('a * 'a -> 'a) -> 'a -> 'a t -> 'a t * 'a

    val sort : 'a ord -> 'a t -> 'a t
    val merge : 'a ord -> 'a t * 'a t -> 'a t
  end