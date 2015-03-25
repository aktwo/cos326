(* NAME: Akshay Kumar 
 * NETID: aktwo@ 
 * PRECEPT: P02
 *)

(*************)
(* PROBLEM 1 *)
(*************)

(* Explain in a comment why each of the following will not type check, 
   uncomment the code and make a small fix so that it does type check *)

(* Problem 1a 
 * Did not typecheck because: elements of a list need to be separated
 * by semi-colons, not by commas.
 *)

let prob1a : int list = [1; 2; 3] ;;


(* Problem 1b 
 * Did not typecheck because: compiler was expecting a tuple
 * containing a string and an int-list, rather than a list 
 * of (string, int) tuples. I added parenthesis to clear this
 * up.
 *)

let prob1b : (string * int) list = [("CS", 50); ("CS", 51)] ;;


(* Problem 1c
 * Did not typecheck because: "::" operator expects the
 * first argument to be type t and the second argument to 
 * be type t-list. In this case, both elements are of the
 * same type.
 *)

let prob1c : float list = 2.0 :: 3.0 :: [4.0; 5.0] ;;

(*************)
(* PROBLEM 2 *)
(*************)

(* Fill in expressions to satisfy the following types: 
 *
 * NOTE: for option and list types, you must go at least one layer deep.
 * example problems:
 *   let x : int option = ??? ;;
 *   let y : int list = ??? ;;
 * incorrect answers:
 *   let x : int option = None ;;
 *   let y : int list = [] ;;
 * possible correct answers:
 *   let x : int option = Some 1 ;;
 *   let y : int list = [1] ;;
 *   let y : int list = [1; 2] ;;
 *   let y : int list = 1 :: [] ;;
 *)

(*>* Problem 2a *>*)

let prob2a : (float * (string * int) option list) list =
  [(3., [Some ("hi", 2)] )]
;;


(*>* Problem 2b *>*)
(* a student is a (name, age option) pair *)

type student = string * int option;;


let prob2b : (student list option * int) list = 
  [(Some [("Akshay", Some 20)], 100)]
;;



(*>* Problem 2c *>*)

let prob2c : (int * int -> int) * (float -> float -> unit) * bool  =
  ((fun (x, y) -> x+y), (fun x y -> print_float (x +. y)), true)
;;


(* Fill in ??? with something that makes these typecheck: *)
(*>* Problem 2d *>*)

let prob2d =
  let rec foo bar =
    match bar with
    | (a, (b, c)) :: xs -> if a then (b + c + (foo xs)) else foo xs
    | _ -> 0
  in foo [(true, (3, 4)); (false, (4, 5))]
;;

(*************)
(* PROBLEM 3 *)
(*************)

(* Consider the following terribly written function: *)

let rec zardoz f ls acc =
  if (((List.length ls) = 1) = true) then (f (List.hd(ls)) (acc))
  else if (((List.length ls) = 0) = true) then acc
  else let hd = List.hd(ls) in
  let tl = List.tl(ls) in
  let ans = f (hd) (acc) in
  let ans = zardoz f tl ans in
    ans
;;

(* Rewrite the code above so that it does the same thing
 * but style-wise is far superior.  
 * Write some assert statements
 * to check that your function is doing the same thing as the original.  
 * Use the COS 326 style guide. *)

let rec myzardoz f ls acc = 
  match ls with
    [] -> acc
  | hd :: [] -> f hd acc
  | hd :: tl -> myzardoz f tl (f (hd) (acc))
;;

let f (x : int) (y : float) : float = float_of_int(x) +. y;;
let ls = [1; 2; 3];;
let acc = 0.1;;

assert ((zardoz f ls acc) = (myzardoz f ls acc));;

(*************)
(* PROBLEM 4 *)
(*************)

(***************************************)
(* Conway's Lost Cosmological Theorem! *)
(***************************************)

(* 

If l is any list of integers, the look-and-say list of s is obtained by 
reading off adjacent groups of identical elements in s. For example, the 
look-and-say list of

l = [2; 2; 2]

is

[3; 2]

because l is exactly "three twos." Similarly, the look-and-say sequence of

l = [1; 2; 2]

is

[1; 1; 2; 2]

because l is exactly "one ones, then two twos."

We will use the term run to mean a maximal length sublist of a list with 
all equal elements. For example,

[1; 1; 1] and [5]

are both runs of the list

[1; 1; 1; 5; 2]

but

[1; 1] and [5; 2] and [1; 2]

are not: 

[1; 1] is not maximal
[5; 2] has unequal elements
[1; 2] is not a sublist.

You will now define a function look and say that computes the 
look-and-say sequence of its argument.  

For full credit your solution should be a linear time solution.

CULTURAL ASIDE:

The title of this problem comes from a theorem about the sequence generated 
by repeated applications of the "look and say" operation. As look and say 
has type int list -> int list, the function can be applied to its own result. 
For example, if we start with the list of length one consisting of just the 
number 1, we get the following first 6 elements of the sequence:

[1]
[1,1]
[2,1]
[1,2,1,1]
[1,1,1,2,2,1]
[3,1,2,2,1,1]

Conway's theorem states that any element of this sequence will "decay" 
(by repeated applications of look and say) into a "compound" made up of 
combinations of "primitive elements" (there are 92 of them, plus 2 
infinite families) in 24 steps. If you are interested in this sequence, 
you may wish to consult [Conway(1987)] or other papers about the 
"look and say" operation.
*)

let look_and_say (xs: int list) : int list = 
  (* helper function to iterate over list and count the 
     number of subsequent elements *)
  let rec count (input : int list) (counter : int) : int list = 
    match input with
      [] -> []
    | hd :: tl -> 
      ( match tl with 
	[] -> counter :: [hd]
      | tl_hd :: tl_tl -> if (hd = tl_hd) then count tl (counter + 1) else (counter :: hd :: (count tl 1)))
  in
  count xs 1
;;

(*************)
(* PROBLEM 5 *)
(*************)

(* 5a. Write a function that flattens a list of lists in to a single
 * list with all of the elements in order. eg:
 *
 * flatten [[1;2;3]; []; [4]; [5;6]] = [1;2;3;4;5;6] 
 *
 *)

let rec flatten (xss:'a list list) : 'a list =

  match xss with 
    [] -> []
  | hd :: tl -> (hd) @ (flatten tl)

;;

(* 5b. Given a matrix (list of lists), return the transpose.
 * The transpose of a matrix interchanges the rows and columns.
 * For example, transpose [[1;2;3];[4;5;6]];;
 * where [1;2;3] and [4;5;6] are the rows,
 * should return [[1;4];[2;5];[3;6]].
 *)
  
let rec transpose (m:int list list) : int list list =
  (* helper function to split each row into a column *)
  let rec split (input : int list) : int list list = 
    match input with 
      [] -> []
    | hd :: tl -> [hd] :: split tl
  in
  (* helper function to concatenate columns *)
  let rec zip (first : int list list) (second : int list list) : int list list = 
    match (first, second) with
      ([], []) -> []
    | (_, []) -> first
    | ([], _) -> second
    | (hd :: tl, hd2 :: tl2) -> (hd @ hd2) :: zip tl tl2
  in
  match m with 
    [] -> []
  | hd :: tl -> zip (split hd) (transpose tl)
;;

(* 5c. Return the list of all permutations of the input list. eg: 
 * perm [1;2;3] = [[1;2;3]; [1;3;2]; [2;1;3]; [2;3;1]; [3;1;2]; [3;2;1]] 
*)
(* test this on small inputs!! *)
let rec perm (items:'a list) : 'a list list =
  (* helper method to generate a list of lists representing all
     possible configurations of "item" within "post_list" *)
  let rec generate (item: 'a) (prev_list: 'a list) (post_list: 'a list) (perm_list: 'a list list) : 'a list list = 
    match (prev_list, post_list) with 
      ([], []) -> [[item]]
    |  (_, []) -> perm_list @ [prev_list @ [item]]
    |  ([], hd :: tl) -> perm_list @ [item :: post_list] @ (generate item [hd] tl perm_list)
    |  (_,  post_hd :: post_tl) -> perm_list @ [prev_list @ [item] @ post_list] @ (generate item (prev_list @ [post_hd]) (post_tl) perm_list)
  in
  (* helper method to insert the item into each sublist using the 
     "insert" method above *)
  let rec insert_over_nested_list (item: 'a) (nested_list: 'a list list) : 'a list list = 
    match nested_list with
      [] -> []
    | hd :: tl -> (generate item [] hd []) @ (insert_over_nested_list item tl)
  in
  match items with
    [] -> []
  | hd :: [] -> [[hd]]
  | hd :: tl -> insert_over_nested_list hd (perm tl)
;;
