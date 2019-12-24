module Seq : Sequence =
  struct
    type 'a seq = 'a array
    type order = Less | Equal | Greater
    type 'a ord = 'a * 'a -> order

    exception IndexOutOfRange
    exception NegativeSize

    let nth l i =  (Array.get l i) handle _ => raise IndexOutOfRange
    let length l = Array.length l
    let toList l = Array.to_list l
    let toString f l = 
      let strArr = Array.map f l in
      let s = Array.fold_left (fun l r -> l^(","^r))"" strArr in
      let ls = (String.length s) -1 in
        "<"^((String.sub s ls)^">")
    
  end
