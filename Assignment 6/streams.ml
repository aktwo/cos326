(***************** Using the Lazy module ******************)
(* Here we provide an alternate implementation of streams using
   OCaml's lazy module. We recommend that you explore the
   documentation at
   http://caml.inria.fr/pub/docs/manual-ocaml/libref/Lazy.html

   In this portion, you will be reimplementing various functions that
   were defined in class and several more ...
*)

(* This file uses OCaml Num library for arbitrary precision arithmetic.
 *
 * To load this file in the OCaml toplevel, you need to start Ocaml 
 * with nums.cma (the Unix library) as an argument: 
 *
 *   % ocaml nums.cma
 *            Objective Caml version 3.11.0
 *   # 
 *
 * To use ocamlbuild, see the associated Makefile.
 *)

open Lazy;;

type 'a stream = Cons of 'a * 'a stream Lazy.t;;

let rec ones = Cons(1, lazy(ones));;

(*>* Problem 2.1.a *>*)
(* Implement the head function *)

let head (s:'a stream) : 'a =
  match s with
  | Cons(hd, _) -> hd
;;

(*>* Problem 2.1.b *>*)
(* Implement map *)

let rec map (f:'a -> 'b) (s:'a stream) : 'b stream =
  match s with 
  | Cons(hd, tl) -> Cons(f hd, lazy(map f (force tl)))
;;

(*>* Problem 2.1.c *>*)
(* Define nats *)

let rec nats = Cons(0, lazy(map ((+) 1) nats)) ;;

(*>* Problem 2.1.d *>*)
(* Write a function nth, which returns the nth element of a
   stream. NOTE: the function nth should be zero-indexed. In other
   words, "nth 0 s" should be equivalent to "head s". *)

let rec nth (n:int) (s:'a stream) : 'a =
  match s with 
  | Cons(hd, tl) -> 
    (
      match n with 
      | 0 -> head s
      | n -> nth (n-1) (force tl)
    )
;;

(*>* Problem 2.1.e *>*)
(* Now suppose we have two int streams s1 and s2 sorted in ascending
   order. We wish to merge these into a single stream s such that s is
   sorted in ascending order and has no duplicates. Implement this
   function. NOTE: This is not a simple merge function. You must
   REMOVE DUPLICATES. *)

let merge (s1:int stream) (s2:int stream) : int stream =
  let rec merge_aux (s1: int stream) (s2: int stream) (last: int) : int stream = 
    match (s1, s2) with
    | (Cons(hd1, tl1), Cons(hd2, tl2)) -> (
      if hd1 < hd2 then 
	(
	  if hd1 = last then merge_aux (force tl1) s2 hd1 
	  else Cons(hd1, lazy(merge_aux (force tl1) s2 hd1))
	)
      else if hd1 = hd2 then 
	(
	  if hd1 = last then merge_aux (force tl1) (force tl2) hd1
	  else Cons(hd1, lazy(merge_aux (force tl1) (force tl2) hd1))
	)
      else
	(
	  if hd2 = last then merge_aux (force tl2) s1 hd2
	  else Cons(hd2, lazy(merge_aux s1 (force tl2) hd2)) 
	)
    )
  in 
  match (s1, s2) with 
  | (Cons(hd1, tl1), Cons(hd2, tl2)) -> (
    if hd1 < hd2 then Cons(hd1, lazy(merge_aux (force tl1) s2 hd1))
    else if hd1 = hd2 then Cons(hd1, lazy(merge_aux (force tl1) (force tl2) hd1))
    else Cons(hd2, lazy(merge_aux s1 (force tl2) hd2))
  )
;;

(*>* Problem 2.1.f *>*)
(* What problems can we run into with this conception of "merge"? What
   if we were to run "merge ones ones"? Answer within the comment. *)

(* 
   Answer: The function would return Cons(1, <lazy>), so we would be
   able to retrieve the head of (merge ones ones). However, when we
   try to retrieve the second element, the function will continue
   searching through the infinite list of ones for an entry that is 
   not one (since merge can never return duplicates), and since it
   will never find such a number, it will fail to terminate.
*)

(*>* Problem 2.1.g *>*)
(* Write a function "scale", which takes an integer "n" and an int
   stream "s", and multiplies each element of "s" by "n". *)

let scale n s = map (fun a -> a*n) s ;;

(*>* Problem 2.1.h *>*)
(* Suppose we wish to create a stream of the positive integers "n" in
   increasing order such that any prime factor of "n" is either 3 or
   5. The first few numbers are 1, 3, 5, 9, 15, 25, 27, ... hopefully
   you get the idea. One way to do this is to run "filter" over
   "nats". But we can do better than that. *)

(* Let "selectivestream" denote the desired stream. Observe that "selectivestream" satisfies the
   following properties

   1. The elements of "scale 3 S" are elements of "selectivestream"
   2. The elements of "scale 5 S" are elements of "selectivestream"
   3. "selectivestream" is the minimal stream (sorted in increasing order) which
   contains "1" and satisfies the above properties

   Think about why properties 1-3 uniquely characterize "selectivestream".
*)

(* Use the above discussion and functions you've already written to
   give a simple definition for "selectivestream". This can be done quite
   elegantly. *)

let rec selectivestream = Cons(1, lazy(merge (scale 3 selectivestream) (scale 5 selectivestream)))

(*>* Problem 2.2 *>*)

(* Define a type for an infinite spreadsheet full of cells with type 'a. 
 *
 * You are free to use any representation of infinite spreadsheets you choose.
 *
 * Each cell in the spreadsheet will have coordinates (i,j).  You can think
 * of a cell with coordinates (i,j) as the cell inhabiting the ith row and
 * jth column of the spreadsheet.  You can assume that (i,j) are both
 * non-negative.  Indices in the spreadsheet start at 0.

 * Coordinates will be represented using OCaml's arbitrary precision Num
 * library.  See here:

   http://caml.inria.fr/pub/docs/manual-ocaml-4.00/libref/Num.html

   Note that you can use +/ -/ */ // for addition, subtraction, 
   multiplication and division on the type num.
 *)
(* Such a spreadsheet will need to support the following operations *)

open Big_int;;

type 'a spread_sheet = 'a stream stream;;

(* we will pretend that ints are infinite precision *)
(* you can assume all coordinates given are non-negative *)
type coordinates = big_int * big_int ;;

(* a spreadsheet containing all zeros *)
let rec zeros : big_int spread_sheet = 
  let rec zeros_row = Cons(big_int_of_int 0, lazy(zeros_row)) in
  Cons(zeros_row, lazy(zeros))

(* return the element at the (i,j) coordinate in ss *)
let get ((i,j):coordinates) (ss:'a spread_sheet) : 'a =   
  let rec get_row (n:big_int) (s:'b stream stream) : 'b stream =
    match s with 
    | Cons(hd, tl) -> 
      (
	if eq_big_int n zero_big_int then head s
	else get_row (sub_big_int n unit_big_int) (force tl)
      )
  in
  let rec get_col (n:big_int) (s:'c stream) : 'c =
    match s with 
    | Cons(hd, tl) -> 
      (
	if eq_big_int n zero_big_int then head s
	else get_col (sub_big_int n unit_big_int) (force tl)
      )
  in
  get_col j (get_row i ss)
;;

(* create a new spreadsheet where the (i,j) element of the spreadsheet
 * contains f i j xij  when xij was the (i,j) element of the input spreadsheet
 *)
let map_all 
    (f:big_int -> big_int -> 'a -> 'b) 
    (ss:'a spread_sheet) 
    : 'b spread_sheet = 
  let rec map_row (i: big_int) (j: big_int) (f:big_int -> big_int -> 'a -> 'b) (row: 'a stream) : 'b stream = 
    match row with 
    | Cons(cell, remaining_row) -> Cons(f i j cell, lazy(map_row i (add_big_int unit_big_int j) f (force remaining_row)))
  in
  let rec map_sheet (i: big_int) (j: big_int) (f:big_int -> big_int -> 'a -> 'b) (ss: 'a spread_sheet) : 'b spread_sheet = 
    match ss with 
    | Cons(row, remaining_sheet) -> 
      Cons(map_row i j f row, lazy(map_sheet (add_big_int unit_big_int i) j f (force remaining_sheet)))
  in 
  map_sheet (zero_big_int) (zero_big_int) f ss
;;

(* create an infinite multiplcation table in which every cell contains the
 * product of its indices *)
let multiplication_table = map_all (fun i j xij -> mult_big_int i j) zeros;;

(* produce a spreadsheet in which cell (i,j) contains the ith element
 * of is and the jth element of js *)
let cross_product (is:'a stream) (js:'b stream) : ('a * 'b) spread_sheet =
  let rec create_row (i: 'a) (js: 'b stream) : ('a * 'b) stream = 
    match js with 
    | Cons(hd, tl) -> Cons((i, hd), lazy(create_row i (force tl)))
  in
  let rec create_sheet (is: 'a stream) (js: 'b stream) : ('a * 'b) spread_sheet = 
    match is with
    | Cons(hd, tl) -> Cons(create_row hd js, lazy(create_sheet (force tl) js))
  in
  create_sheet is js
;;

(* KARMA (Optional): Cryptographic keys are often generated from products
 * of large prime big_intbers.  Use cross_product and map_all to help you
 * compute a spreadsheet that for all pairs of primes p and q contains
 * a square in the spreadsheet that contains p*q.
 * Recall that to generate all primes, you can use an algorithm 
 * called the Sieve of Eratosthenes.
 *
 * see here: 
 * http://en.wikipedia.org/wiki/Sieve_of_Eratosthenes
 * http://www.cs.princeton.edu/~dpw/courses/cos326-12/lec/code/streams.tgz
 *)

let product_of_primes : big_int spread_sheet = failwith "unimplemented";;
