module type S = sig
  type 'a t
  val length : 'a t -> int
  val empty : unit  ->'a t
  val cons : 'a -> 'a t -> 'a t
  val singleton : 'a -> 'a t
  val append : 'a t -> 'a t -> 'a t
  val tabulate : (int -> 'a) -> int -> 'a t
  val nth : 'a t -> int -> 'a
  val filter : ('a -> bool) -> 'a t -> 'a t
  val map : ('a -> 'b) -> 'a t -> 'b t
  val reduce : ('a -> 'a -> 'a) -> 'a -> 'a t -> 'a
  val map_reduce : ('a -> 'b) -> 'b -> ('b -> 'b -> 'b) -> 'a t -> 'b
  val repeat : 'a -> int -> 'a t
  val flatten : 'a t t -> 'a t
  val zip : ('a t * 'b t) -> ('a * 'b) t
  val split : 'a t -> int -> 'a t * 'a t
end

(*******************************************************)
(* Sequential Sequences Based on a List Representation *)
(*******************************************************)

module ListSeq : S = struct

type 'a t = 'a list

let length = List.length

let empty () = []

let cons (x:'a) (s:'a t) = x::s

let singleton x = [x]

let append = List.append

let tabulate f n =
  let rec helper acc x =
    if x = n then List.rev acc
    else helper ((f x)::acc) (x+1) in
  helper [] 0

let nth = List.nth

let filter = List.filter

let map = List.map

let reduce = List.fold_left

let map_reduce l e n s = reduce n e (map l s)

let repeat x n =
  let rec helper x n acc =
    if n = 0 then acc else helper x (n-1) (x::acc) in
  helper x n []

let flatten = List.flatten

let zip (s1,s2) = List.combine s1 s2

let split s i =
  let rec helper s i acc =
    match s,i with
      | _,0 -> (List.rev acc,s)
      | [],_ -> failwith "split"
      | h::t,_ -> helper t (i-1) (h::acc) in
  helper s i []

end

(**********************)
(* Parallel Sequences *)
(**********************)

open Par

module ParSeq : S = struct
    
exception Invalid_argument 

type 'a t = 'a Arr.t option

let length s = 
  match s with 
  | None -> 0 
  | Some a -> Arr.length a

let empty () = None

let singleton x = Some (Arr.make 1 x)

let nth s =
  match s with
  | None -> raise Invalid_argument
  | Some a -> Arr.get a

let append (s1: 'a t) (s2: 'a t): 'a t = 
  match (s1, s2) with 
  | (None, _) -> s2
  | (_, None) -> s1
  | (_, _) -> (
    let n1 = length s1 in
    let n2 = length s2 in
    let copyTo toArray fromSeq offset i = 
      Arr.set toArray (i + offset) (nth fromSeq i)
    in
    let newArray = Arr.make (n1+n2) (nth s1 0) in
    let _ = par (multi_par (copyTo newArray s1 0)) n1 (multi_par (copyTo newArray s2 n1)) n2 in
    Some newArray
  )

let cons (x:'a) (s:'a t) =
  append (singleton x) s

let tabulate f n =
  if (n < 0) then raise Invalid_argument
  else 
  let helper toArray i =
    Arr.set toArray i (f i)
  in
  let newArray = Arr.make n (f 0) in
  let _ = multi_par (helper newArray) n in
  Some newArray

let map f s =
  match s with
  | None -> None
  | Some a -> (
    let newArray = Arr.make (length s) (f (nth s 0)) in
    let mapCopyTo theArray i = Arr.set theArray i (f (Arr.get a i)) in
    let _ = multi_par (mapCopyTo newArray) (length s) in
    Some newArray
  )

let repeat x n =
  if (n < 0) then raise Invalid_argument
  else if (n = 0) then None 
  else Some (Arr.make n x)

let zip (s1, s2) =
  match (s1, s2) with
  | (None, None) -> None 
  | (None, _) -> raise Invalid_argument
  | (_, None) -> raise Invalid_argument
  | (_, _) -> (
    let n1 = length s1 in
    let n2 = length s2 in
    if (n1 <> n2) then raise Invalid_argument
    else
      let helper toArray i =
        Arr.set toArray i ((nth s1 i), (nth s2 i))
      in
      let newArr = Arr.make n1 ((nth s1 0), (nth s2 0)) in
      let _ = multi_par (helper newArr) n1 in 
      Some newArr
  )

let split s i =
  if (i < 0) then raise Invalid_argument
  else
  match s with
  | None -> raise Invalid_argument
  | Some _ -> (
    if (i = 0) then (None, s)
    else if (i = (length s)) then (s, None)
    else (
      let n = length s in
      let helper toArray offset j =
        Arr.set toArray j (nth s (j+offset))
      in
      let newArr1 = Arr.make i (nth s 0) in
      let newArr2 = Arr.make (n-i) (nth s 0) in
      let _ = par (multi_par (helper newArr1 0)) i (multi_par (helper newArr2 i)) (n-i)  in
      (Some newArr1, Some newArr2)
    )
  )

let reduce c b s = 
  match s with
  | None -> b
  | Some _ -> (
    let rec aux seq = 
      match seq with 
      | None -> failwith "impossible"
      | Some _ -> (
        let n = length seq in
        match n with
        | 0 -> failwith "impossible"
        | 1 -> c b (nth seq 0)
        | 2 -> c (nth seq 0) (nth seq 1)
        | _ -> (
          let (seq1, seq2) = split seq (n / 2) in
          let (r1, r2) = parp aux seq1 aux seq2 in
          c r1 r2
        )
      )
    in
    aux s
  )

let map_reduce l e n s = reduce n e (map l s)

let flatten ss = reduce append (empty ()) ss  

let filter p s =
  let mapFunction x =
    if (p x) then singleton x
    else empty ()
  in
  flatten (map mapFunction s)

(* -1 if first is less than second *)
let rec sort f s = 
  let n = length s in
  match n with 
  | 0 -> empty ()
  | 1 -> s
  | _ -> (
    let pivot = nth s 0 in
    let getLeft = filter (fun i -> 
      match f i pivot with 
      | -1 -> true
      | _ -> false
    ) in
    let getRight = filter (fun i -> 
      match f i pivot with
      | 1 -> true
      | _ -> false
    ) in
    let getMid = filter (fun i -> 
      match f i pivot with 
      | 0 -> true 
      | _ -> false
    ) in
    let ((left, right), mid) = parp (fun a -> parp getLeft a getRight a) s getMid s in
    let (sortedLeft, sortedRight) = parp (sort f) left (sort f) right in
    append sortedLeft (append mid sortedRight)
    )

end
