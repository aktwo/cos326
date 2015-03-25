open Memoizer
open Timing

type base = Base.base;;
type dna = Base.dna;;

(* slow lcs *)
let rec slow_lcs ((s1,s2) : dna * dna) : dna =
  match (s1,s2) with 
      ([], _) -> []
    | (_, []) -> []
    | (x :: xs, y :: ys) ->
      if Base.eq x y then
	x :: slow_lcs (xs, ys)
      else
	Base.longer_dna_of (slow_lcs (s1, ys)) (slow_lcs (xs, s2))
;;

(* A potentially useful module *)
module DnaPairOrder : Map.OrderedType with type t = dna * dna =
struct
    type t = dna * dna

    let rec compare_dna x' y' : int = 
        match x',y' with 
          [],[] -> 0
        | [], xs -> -1
        | xs, [] -> 1
        | x::xs, y::ys -> 
	  (match Base.compare x y with
	      0 -> compare_dna xs ys
            | other -> other)
	    

    (* implements a lexicographic ordering: 
     * compare the second components only if first components are equal *)
    let compare (a, b) (c, d) =
      match compare_dna a c with
	  0 -> compare_dna b d
        | other -> other
     
end;;

(* Task 4.4 *)

(* implement fast_lcs using your automatic memoizer functor! 
 * doing so will of course require proper creation of modules and
 * use of functors *)
let fast_lcs (ds : dna * dna) : dna =  
  let module M = Memoizer(Map.Make(DnaPairOrder)) in
  let intermediate_lcs (slow_lcs: (dna * dna) -> dna) ((s1,s2) : dna * dna) : dna =
    match (s1,s2) with 
      ([], _) -> []
    | (_, []) -> []
    | (x :: xs, y :: ys) ->
      if Base.eq x y then
	x :: slow_lcs (xs, ys)
      else
	Base.longer_dna_of (slow_lcs (s1, ys)) (slow_lcs (xs, s2))
  in 
  M.memo intermediate_lcs ds
;;

(* Task 4.5 *)

(* Implement some experiment that shows performance difference
 * between slow_lcs and fast_lcs. (Print your results.)     
 * Explain in a brief comment what your experiment shows.        *)

(* This experiment is modeled after the testing code in fib.ml. I 
   picked an arbitrary set of DNA subsequences to run experiments 
   on and displayed the slow_lcs and fast_lcs times side by side
   in order to demonstrate that fast_lcs is in fact faster as a 
   result of memoization. The dna_list list in the main () method
   can be modified to perform more or fewer tests depending on user
   preference. In this case, I only used a few testing cases in order
   to make the tests run more quickly. *)

let print_header () = 
  print_string ("---- (string length) ---- -------- (secs) --------\n");
  print_string ("        Input Size        Slow LCS       Fast LCS \n");
  print_string ("------------------------- ------------------------\n"); 
;;

let print_row first second slow fast = 
  let space () = print_string "         " in
  let float f = Printf.printf "%6.4f" f in 
  let print_pair first second = print_string 
    ("(" ^ string_of_int (String.length first) ^ ", " ^
	string_of_int (String.length second) ^ ")") in
  space ();
  print_pair first second;
  space ();
  float slow;
  space ();
  float fast;
  print_newline();
;;

let experiment ((first, second) : string * string) =
  let first_dna = Base.dna_from_string first in
  let second_dna = Base.dna_from_string second in
  let slow_time = time_fun slow_lcs (first_dna, second_dna) in
  let fast_time = time_fun fast_lcs (first_dna, second_dna) in
  print_row first second slow_time fast_time
;;

let main () =
  (* Modify this list of DNA subsequence pairs to modify the
     experiments run *)
  let dna_list = 
    [("AGATTAGATT", "AGTCCAGTAGTCCAGT"); 
     ("AGATTAGATTAGATT", "AGTCCAGTAGTCCAGTAGTCCA");
     ("AGATTAGATTAGATT", "AGTCCAGTAGTCCAGTAGTCCAGT");
     ("AGATTAGATTAGATTAGATT", "AGTCCAGTAGTCCAGT");] in
  print_header ();
  List.iter experiment dna_list 
;;

main();;
