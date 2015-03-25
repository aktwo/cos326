(* Box office analysis *)

(* Contents:
    -- the movie type
    -- the studio_gross type
    -- functions for querying and transforming lists of movies
*)

(* a movie is a tuple of (title, studio, gross in millions, year) *)
type movie = string * string * float * int;;

(* a studio_gross is a pair of (studio, gross in millions) *)
type studio_gross = string * float;;

(* call bad_argument if your function receives a bad argument *)
(* do not change this exception or function                   *)
exception Bad_arg of string
let bad_arg (s:string) = raise (Bad_arg s)

(* a useful debugging routine *)
let debug s = print_string s; flush_all()

(* *** DO NOT CHANGE DEFINITIONS ABOVE THIS LINE! *** *)

(* you may add "rec" after any of the let declarations below that you
 * wish if you find doing so useful. *)

(* find the average gross of the movies in the list                  *)
(* return 0.0 if the list is empty                                   *)
(* hint: you may need to use functions float_of_int and int_of_float *)
(* hint: if you don't know what those functions do,                  *)
(*       type them in to ocaml toplevel                              *)
(* hint: recall the difference between +. and + also 0. and 0        *)
let average (movies : movie list) : float = 
  let length = List.length(movies) in
  (* helper function to sum the revenue of all movies in the list *)
  let rec sum (movies : movie list) : float = 
    match movies with 
      [] -> 0.
    | (_, _, gross, _) :: tl -> gross +. sum tl
  in
  if (length = 0) then 0. else (sum movies) /. (float_of_int length)
;;

(* return a list containing only the movies from the given decade *)
(* call bad_arg if n is not 20, 30, ..., 90, 00, 10               *)
(* Treat 0 as 00 (this is unavoidable as 00 is not represented *)
(*   differently from 0).                                      *)
(* Note any years outside the range 1920-2019 will always be discarded *)
(* but should not raise an error condition *)
let decade (n:int) (ms:movie list) : movie list =   
  match n with 
    20 -> List.filter (fun (_, _, _, x) -> (x >= 1920 & x <= 1929)) ms 
  | 30 -> List.filter (fun (_, _, _, x) -> (x >= 1930 & x <= 1939)) ms 
  | 40 -> List.filter (fun (_, _, _, x) -> (x >= 1940 & x <= 1949)) ms 
  | 50 -> List.filter (fun (_, _, _, x) -> (x >= 1950 & x <= 1959)) ms 
  | 60 -> List.filter (fun (_, _, _, x) -> (x >= 1960 & x <= 1969)) ms 
  | 70 -> List.filter (fun (_, _, _, x) -> (x >= 1970 & x <= 1979)) ms 
  | 80 -> List.filter (fun (_, _, _, x) -> (x >= 1980 & x <= 1989)) ms 
  | 90 -> List.filter (fun (_, _, _, x) -> (x >= 1990 & x <= 1999)) ms 
  | 0  -> List.filter (fun (_, _, _, x) -> (x >= 2000 & x <= 2009)) ms 
  | 10 -> List.filter (fun (_, _, _, x) -> (x >= 2010 & x <= 2019)) ms 
  | _  -> bad_arg "Invalid year!"
;;

(* return the first n items from the list *)
(* if there are fewer than n items, return all of them *)
let rec take (n:int) (l:'a list)  : 'a list =
  if n < 0 then bad_arg "Argument n must be non-negative!"
  else
  match (n, l) with 
    (0, _) -> []
  | (_, []) -> []
  | (_, hd :: tl) -> hd :: take (n-1) (tl)
;;

(* return everything but the first n items from the list *)
(* if there are fewer than n items, return the empty set *)
let rec drop (n:int) (l:'a list)  : 'a list =
  if (n < 0) then bad_arg "Argument n must be non-negative!"
  else
    match (n, l) with 
      (0, _) -> l
    | (_, []) -> []
    | (_, hd :: tl) -> drop (n-1) (tl)
;;

(* return a list [x1; x2; ...; xn] with the same elements as the input l
   and where:
     leq xn xn-1
     ...
     leq x3 x2
     leq x2 x1
     are all true
*)
(* hint: define an auxiliary function "select" *)
(* hint: select probably should return a pair.  Of what?  Think functionally. *)
type 'a less = 'a -> 'a -> bool;;
let rec selection_sort (leq:'a less) (l:'a list) : 'a list =
  let rec select (leq: 'a less) (l : 'a list) : ('a option * 'a list) = 
    match l with
      [] -> (None, [])
    | hd :: [] -> (Some hd, [])
    | hd :: tl -> 
      (let retValue = select leq tl in
       match retValue with 
	 (None, _) -> (None, []) 
       | (Some value, list) -> if (leq hd value) then (Some hd, value :: list) else (Some value, hd :: list))
  in
  match (select leq l) with 
    (None, _) -> []
  | (Some min, list) -> min :: selection_sort leq list
;;

(* return list of movies sorted by gross (largest gross first) *)
let sort_by_gross (movies : movie list) : movie list = 
  selection_sort (fun (_, _, x, _) (_, _, y, _) -> x >= y) movies
;;

(* return list of movies sorted by year produced (largest year first) *)
let sort_by_year (movies : movie list) : movie list = 
  selection_sort (fun (_, _, _, x) (_, _, _, y) -> x >= y) movies
;;

(* sort list of (studio, gross in millions) by gross in millions 
 * with the largest gross first *)
let sort_by_studio (studio_grosses : studio_gross list) : studio_gross list = 
  selection_sort (fun (_, x) (_, y) -> x >= y) studio_grosses
;;

(* given list of movies,
 * return list of pairs (studio_name, total gross revenue for that studio)  *)
let by_studio (movies:movie list) : studio_gross list =
  let rec add_movie_to_studio_list (movie: movie) (studio_list: studio_gross list) : studio_gross list = 
    let (_, movie_studio, movie_gross, _) = movie in
    match studio_list with 
      [] -> studio_list @ [movie_studio, movie_gross]
    | hd :: tl -> let (current_studio, total_gross) = hd in 
		  if (current_studio = movie_studio) 
		  then (current_studio, total_gross +. movie_gross) :: tl 
		  else hd :: add_movie_to_studio_list movie tl
  in
  let rec process_movies (movies: movie list) (old_studio_list: studio_gross list) = 
    match movies with
      [] -> old_studio_list
    | hd :: tl -> process_movies tl (add_movie_to_studio_list hd old_studio_list)
  in
  process_movies movies []
;;

(***********)
(* Testing *)
(***********)

(* Augment the testing infrastructure below as you see fit *)

(* Test Data *)

let data1 : movie list = [
  ("The Lord of the Rings: The Return of the King","NL",377.85,2003)
];;

let data2 : movie list = [
  ("The Lord of the Rings: The Return of the King","NL",377.85,2003);
  ("The Hunger Games","LGF",374.32,2012)
];;

let data3 : movie list = [
  ("Harry Potter and the Sorcerer's Stone","WB",317.57555,2001);
  ("Star Wars: Episode II - Attack of the Clones","Fox",310.67674,2002);
  ("Return of the Jedi", "Fox", 309.306177, 1983)
];;

let data4 : movie list = [
  ("The Lord of the Rings: The Return of the King","NL",377.85,2003);
  ("The Hunger Games","LGF",374.32,2012);
  ("The Dark Knight","WB",533.34,2008);
  ("Harry Potter and the Deathly Hallows Part 2","WB",381.01,2011)
];;

(* Assertion Testing *)

(* Uncomment the following when you are ready to test your take routine *)

assert(take 0 data4 = []);;
assert(take 1 data1 = data1);;
assert(take 2 data4 = data2);;
assert(take 5 data2 = data2);;
assert(take 2 data2 = data2);;

(* Additional Testing Infrastructure *)

let stests : (unit -> movie list) list = [
  (fun () -> sort_by_gross data1);
  (fun () -> sort_by_gross data2);
  (fun () -> sort_by_gross data3);
  (fun () -> sort_by_gross data4)
];;

let check (i:int) (tests:(unit -> 'a) list) : 'a =
  if i < List.length tests && i >= 0 then
    List.nth tests i ()
  else
    failwith ("bad test" ^ string_of_int i)
;;