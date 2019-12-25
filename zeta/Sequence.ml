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
      let s = Array.fold_left (fun l r -> l^(","^r)) "" strArr in
      let ls = (String.length s) -1 in
        "<"^((String.sub s ls)^">")
    let equal f l r = if (Array.length l) != (Array.length r) then false else
      let boolArr = Array.map2 (fun el er -> f (el,er)) l r in
        Array.fold_left (fun a b -> a && b) true boolArr

    let empty = [||]
    let singleton e = [|e|]
    let tabulate f i = (Array.init i f) handle _ => raise NegativeSize
    let fromList l = Array.of_list l

    let rev l = 
      let le = Array.length l in
        Array.init le (fun e -> Array.get l (le-e-1)) 
    let append (l,r) = Array.append l lr
    let flatten l = Array.fold_left (fun a b -> Array.append a b) [||] l

    let filter 
  end
