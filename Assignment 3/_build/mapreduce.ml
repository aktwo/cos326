(* 

Name: Akshay Kumar
Email: aktwo@princeton.edu
Minutes Spent on Problem 1.1: ~2 hours
Minutes Spent on Problem 1.2: ~2 hours

(You aren't in any way graded on the number of minutes spent; 
 we are just trying to calibrate for future versions of the class)

Comments/Problems/Thoughts on this part of the assignment:
It was annoying at first, but being forced to only use map/reduce
actually made me a lot more comfortable with the concepts of 
recursion. Feel much better at OCAML after completing this
assignment.
*)

(* This part of the assignment uses the following functions 
 * If you forget how they work, look them up:
 * http://caml.inria.fr/pub/docs/manual-ocaml/libref/List.html
 *)
let map : ('a -> 'b) -> 'a list -> 'b list = List.map;;

let filter : ('a -> bool) -> 'a list -> 'a list = List.filter;;

let foldr : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b = List.fold_right;;

let foldl : ('b -> 'a -> 'b) -> 'b -> 'a list -> 'b = List.fold_left;;

(* reduce is equivalent to List.fold_right, 
 * only its args are ordered differently *)
let rec reduce (f:'a -> 'b -> 'b) (u:'b) (xs:'a list) : 'b =
  match xs with
    | [] -> u
    | hd::tl -> f hd (reduce f u tl) ;;

(***********************************************)
(******            1.1 WARM UP            ******)
(***********************************************)

(* Solve each problem in this part using map, reduce, foldl, foldr or filter.
 * Map, filter, reduce, foldl, foldr are an example of a *combinator library*:
 * a library of higher-order functions used to solve problems in a particular
 * domain.  In this case, the domain is list-processing.  However, there are 
 * countless other useful combinator libraries.  The goal is to get used to 
 * thinking about how to decompose complex functions in to smaller, simpler, 
 * orthogonal functional components.  The smaller, simpler functional 
 * components can be constructed directly using the combinator library.
 *
 * Note: foldl is slightly more efficient than foldr because it is tail
 * recursive.  (We will explain what that means later in the course.)
 * Hence, solutions that use foldl are typically superior to solutions
 * that use foldr, all other things being equal.  Thus you should try to 
 * use foldl where you can instead of foldr (but take care to retain good 
 * style -- a horribly convoluted, incomprehensible function that uses 
 * foldl is worse than an elegant one that uses foldr).
 *
 * In these problems, you are not allowed to use the "rec" keyword in 
 * your solution.  A solution, even a working one, that uses explicit 
 * recursion via "rec" will receive little to no credit.  You may write
 * useful auxiliary functions; they just may not be recursive.
 *
 * You are also not allowed to use other functions from the list library
 * such as sort, concat or flatten.  (You are allowed to recode those
 * functions yourself using map, filter, fold if you find it necessary.)
 *
 *)

(*>* Problem 1.1.a *>*)

(*  negate_all : Flips the sign of each element in a list *)
let negate_all (nums:int list) : int list =
  map (fun x -> -x) nums
;;

(* Unit test example.  Uncomment after writing negate_all *)
assert ((negate_all [1; -2; 0]) = [-1; 2; 0]);;

(*>* Problem 1.1.b *>*)

(*  sum_rows : Takes a list of int lists (call an internal list a "row").
 *             Returns a one-dimensional list of ints, each int equal to the
 *             sum of the corresponding row in the input.
 *   Example : sum_rows [[1;2]; [3;4]] = [3; 7] *)
let sum_rows (rows:int list list) : int list =
  let rec sum_list (row: int list) : int = 
    match row with
    | [] -> 0
    | hd :: tl -> hd + sum_list tl
  in
  map sum_list rows
;;

assert(sum_rows [[1;2]; [3;4]] = [3; 7]);;

(*>* Problem 1.1.c *>*)

(*  limit_range : Returns a list of numbers in the input list within a
 *                  given range (inclusive), in the same order they appeared
 *                  in the input list.
 *       Example : limit_range [1;3;4;5;2] (1,3) = [1;3;2] *)
let limit_range (nums:int list) (range:int * int) : int list =
  let (a, b) = range in
  let greater_than = filter (fun x -> x >= a) nums in
  filter (fun x -> x <= b) greater_than
;;

assert(limit_range [1;3;4;5;2] (1,3) = [1;3;2]);;

(*>* Problem 1.1.d *>*)

(*  num_occurs : Returns the number of times a given number appears in a list.
 *     Example : num_occurs 4 [1;3;4;5;4] = 2 *)
let num_occurs (n:int) (nums:int list) : int =
  let count_occurence (n:int) (current_count:int) (current_num:int) : int =
    if (current_num = n)
    then current_count + 1
    else current_count
  in
  foldl (count_occurence n) 0 nums
;;

assert(num_occurs 4 [1;3;4;5;4] = 2);;

(*>* Problem 1.1.e *>*)

(*  super_sum : Sums all of the numbers in a list of int lists
 *    Example : super_sum [[1;2;3];[];[5]] = 11 *)

let sum_list (start: int) (nlist : int list) : int = foldl (+) start nlist;;

let super_sum (nlists:int list list) : int =
  let sum_list (start:int) (nlist : int list) : int = foldl (+) start nlist in
  foldl sum_list 0 nlists
;;

assert(super_sum [[1;2;3];[];[5]] = 11);;

(****************************************************)
(**********       1.2 A Bigger Challenge   **********)
(****************************************************)

(*
 * Note: some of these questions may be challenging.  
 * Don't neglect Part 2 of this assignment because you are working on
 * these problems.
 *)

(*>* Problem 1.2.a *>*)

(* min2: returns the second-smallest element of a list, when put into 
 * sorted order. Note that if list contains duplicates, the second-smallest 
 * element and the smallest element may be identical; your code should return 
 * it.
 *
 * Example: min2 [2110; 4820; 3110; 4120] = 3110.
 * Example: min2 [2110; 4820; 2110; 4120] = 2110.
 *
 * For full credit, use a fold function, do not sort the list and do not
 * use the rec keyword (aside from using folds). 
 *
 * You will receive partial credit if you use explicit recursion instead of
 * a fold.
 *
 * If the list contains 0 or 1 elements, call (invalid_arg s) with a helpful
 * string s. See the Pervasives library for the invalid_arg function and 
 * other useful exception-raising functions:
 *
 * http://caml.inria.fr/pub/docs/manual-ocaml/libref/Pervasives.html
*)

let min2 (xs:int list) : int = 
  let remove_min (current : (int) * (int list)) (to_process : int) : ((int) * (int list)) = 
    match (current, to_process) with
    | ((x, []), y) -> if (x < y) then (x, [y]) else (y, [x])
    | ((x, list), y) -> if (x < y) then (x, y :: list) else (y, x :: list)
  in
  match xs with 
    [] -> invalid_arg "Input list has fewer than 2 elements!"
  | hd :: tl -> let (min, list) = foldl remove_min (hd, []) tl 
		in match list with
		| [] -> invalid_arg "Input list has fewer than 2 elements!"
		| new_hd :: new_tl -> let (min_2, _) = foldl remove_min(new_hd, []) new_tl
				      in min_2
;;

assert(min2 [2110; 4820; 3110; 4120] = 3110);;
assert(min2 [2110; 4820; 2110; 4120] = 2110);;
  
(*>* Problem 1.2.b *>*)

(* consec_dedupe : removes consecutive duplicate values from a list. 
 * More specifically, consec_dedupe has two arguments:
 *  eq is a function equiv representing an equivalence relation
 *  xs is a list of elements. 
 * It returns a list containing the same elements as lst, but without 
 * any duplicates, where two elements are considered equal if applying eq 
 * to them yields true.
 *
 * Example: consec_dedupe (=) [1; 1; 1; 3; 4; 4; 3] = [1; 3; 4; 3].
 *
 * Example: 
 *
 * let nocase_eq (s1:string) (s2:string) : bool =
 *   (String.uppercase s1) = (String.uppercase s2)
 * ;;
 * 
 * consec_dedupe nocase_eq ["hi"; "HI"; "bi"] = ["hi"; "bi"]
 *  
 * (When consecutive duplicates are not exactly syntactically equal
 * as above, it does not matter which of the duplicates are discarded.)
 *
 * Again, for full credit, do not use explicit recursion (the rec keyword),
 * but instead use foldr or foldl (or both).
 *
 * Partial credit will be given to solutions that do use explicit recursion.
 *)

let consec_dedupe (eq:'a -> 'a -> bool) (xs:'a list) : 'a list =
  let add_item (eq: 'a -> 'a -> bool) (current_list : 'a list) (current_item : 'a) : 'a list = 
    match current_list with 
    | [] -> [current_item]
    | hd :: tl -> 
      if (eq hd current_item) 
      then current_list
      else current_item :: current_list
  in
  match xs with 
  | [] -> []
  | hd :: tl -> foldl (add_item eq) [] (hd :: tl)
;;

(*>* Problem 1.2.c *>*)

(* prefixes: return a list of all non-empty prefixes of a list, 
 * ordered from shortest to longest.

    Example: prefixes [1;2;3;4] = [[1]; [1;2]; [1;2;3]; [1;2;3;4]].
*)

let prefixes (xs: 'a list) : 'a list list =

  let get_last (initial : 'a list) (current_list : 'a list) : 'a list = 
    match current_list with
      [] -> []
    | x -> x
  in
  let generate (total_list : 'a list list) (last_entry : 'a) : 'a list list = 
    total_list @ [((foldl get_last [] total_list) @ [last_entry])]
  in
  foldl generate [] xs
;;

assert(prefixes [1;2;3;4] = [[1]; [1;2]; [1;2;3]; [1;2;3;4]]);;

(*>* Problem 1.2.d *>*)

(* k_sublist : Given a list of integers nums and an integer k, 
 * the function k_sublist computes the contiguous sublist of length k 
 * whose elements have the largest sum.

    Example: k_sublist [1; 2; 4; 5; 8] 3 = [4; 5; 8].

 * raise invalid_arg "k_sublist" if the length of nums is less than k
 *)

let k_sublist (nums: int list) (k:int) : int list =
  (* throw an error if the nums has a length smaller than k *)
  let length = foldl (fun x -> (fun y -> x + 1)) 0 nums in
  if (length < k) 
  then invalid_arg "k_sublist"
  else 
    (* helper function to generate the initial k-element sublist of nums *)
    let generate_initial_list 
	(length_init_list_and_remaining_list : int * (int list) * (int list)) 
      (new_element : int) : int * (int list) * (int list) = 
      match length_init_list_and_remaining_list with
      | (0, x, y) -> (0, x, y @ [new_element])  
      | (n, x, y) -> (n-1, x @ [new_element], y)
    in  
    (* helper function to generate a list of all k-length sublists of nums *)
    let generate_list_of_lists 
	(total_list_and_curr_list : (int list list) * (int list)) 
	(new_element : int) : (int list list) * (int list) = 
      match total_list_and_curr_list with
      | (x, hd :: tl) -> (x @ [tl @ [new_element]], tl @ [new_element])
      | (x, []) -> (x, [])
    in
    (* helper function to find the sum of a list *)
    let sum (input : int list) : int = 
    foldl (+) 0 input
    in
    (* helper function to find the list in a list-of-lists that has
     * the greatest sum *)
    let find_max (curr_max : int list) (new_list : int list) : int list = 
      if (sum curr_max) < (sum new_list) 
      then new_list
      else curr_max
    in
    let (length, init, remain) = foldl (generate_initial_list) (k, [], []) nums in
    let (list_of_lists, _) = foldl (generate_list_of_lists) ([init], init) remain in
    foldl (find_max) (init) list_of_lists
;;

assert(k_sublist [1; 2; 4; 5; 8] 3 = [4; 5; 8]);;

(*>* Problem 1.2.e *>*)
(* flatten : write a function that flattens a list of lists in to a single
 * list with all of the elements in order. eg:
 *
 * flatten [[1;2;3]; []; [0]; [4;5]] == [1;2;3;0;4;5] 
 *
 * In the last assignment you wrote this function with recursion. Now, 
 * do it without recursion, using folds.
 *)

let flatten (xss:'a list list) : 'a list =
  foldl (@) [] xss
;;
