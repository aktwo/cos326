(*** COS 326 Problem Set 1 ***)
(*** Akshay Kumar ***)
(*** aktwo@ ***)

let undefined : unit -> 'a = fun () -> failwith "undefined" ;;

(* 1. Please define these variables with the appropriate values.
 * Be sure that these statements all type-check after editing them.
 * You can do this by hitting Ctrl+c and then Ctrl+e in Emacs, or by
 * compiling with "ocamlbuild" in the terminal emulator *)

(* 1.a. Create a string with your first name *)
let name : string = "Akshay";;

(* 1.b. Modify that string to contain both your first and last names *)
let name : string = name ^ " Kumar";;

(* 1.c. Create a string containing your email address *)
let email : string = "aktwo@princeton.edu";;

(* 1.d. Replace (Other "...") in class_year with the appropriate item below *)
(* ie: replace (Other "...") with Sophomore or Junior for example *)
type year = Freshman | Sophomore | Junior | Senior | Other of string;;

let class_year : year = Senior;;

(* 1.e. Replace the .... with what you're excited about in this course *)
let exciting : string = "I'm excited about learning functional programming!";;

let print = Printf.printf;;

let print_survey = 
  let string_year = 
    (match class_year with
       | Freshman -> "2017"
       | Sophomore -> "2016"
       | Junior -> "2015"
       | Senior -> "2014"
       | Other s -> "Other: " ^ s
    ) in
    (print "----------------------------------------\n";
     print "Name: %s\n\n" name;
     print "Email: %s\n\n" email;
     print "Year: %s\n\n" string_year; 
     print "%s\n\n" exciting;
     print "----------------------------------------\n\n";);;

(* Problem 2 - Fill in types:
 * Replace each ??? with the appropriate type of the corresponding expression.
 * Be sure to remove the comments from each subproblem and to type check it
 * before submission. 
 * Note that the expressions might not do anything useful -- and in fact 
 * might even display interesting problems! -- but all you should do is fill 
 * in the ???s to make them type check. *)

(* Problem 2a *)

let prob2a : string  = let greet y = "Hello " ^ y in greet "World!" ;;


(* Problem 2b *)

let prob2b : float = float_of_int( int_of_float(2.2 +. 7.7)) ;;


(*>* Problem 2c *>*)

let rec prob2c (x : char) : char =
  prob2c ( if true then prob2c x else 'h')


(*>* Problem 2d *>*)

let rec prob2d (y : bool) (z : bool) : bool =
   prob2d (prob2d z y) (not y);;


(* Explain in a comment why each of 3a, 3b, 3c, 3d will not compile
 * and change the code in some small way so that it does. Do not
 * change the top-level type associated with the expression. *)

(*>* Problem 3a *>*)
(* Because 3.9 is a float while 4 is an int. I added a "." to 
 * the end of 4 to make the two values of the same type. *)

let prob3a : bool = 
  let compare (x) (y) = x < y in 
  compare 3.9 4. 
;;


(*>* Problem 3b *>*)
(* Because there are nested functions (i.e. binary operators) in 
 * the function call of "aux" that need to be separated by parens. *)

let prob3b : int = 
  let fib (n) =
   let rec aux (n) (y) (x) =
    if n <= 0 then x 
    else aux (n-1) (x+y) y
   in
   aux n 1 0
  in
  fib 10
;;


(*>* Problem 3c *>*)
(* Because sumTo is recursive and therefore needs a "rec" statement 
 * after its let declaration. *)

let prob3c : int =
  let rec sumTo (n:int) : int =
  if n <= 0 then 0
  else n + sumTo (n-1)
  in
  sumTo 10
;;


(*>* Problem 4 *>*)
(* 4a: Fill in the ??? with an expression that uses x and y and has
 * the right type *)

let prob4a =
  let u = 32.0 in 
  let v = 28.0 in
  let square w = w *. w in
  let boff (x) (y) = square x +. square y in
  let d = sqrt (boff u v) in
  int_of_float d
;;


(* 4b: Replace each ?? with the type of the corresponding expression,
 * and write a function f that has the correct type siguanture. Explain
 * in a comment a problem that remains with the function prob4b *)


let f (a:int) (b:int) : float  = float_of_int a +. float_of_int b
;;

(* The problem with this function is that it does not terminate on 
 * any input. *)

let rec prob4b (x:float) (y:int) : int =
  prob4b (f y 4) (int_of_float x)
;;


(* 4c:  Is it possible to find types for the argument and result that
 * make the function forever type check?
 *
 * Either give correct types or explain why it is impossible:
 * 
 * This is impossible because the function will always have 
 * some output type, which cannot be included in the input type and thus
 * there is no set of types for which "forever forever" is valid.
 *
 *)
(*
let rec forever (x: ???) : ??? =
  forever forever
;; 
*)

(*>* Problem 5 *>*)

(* few_divisors n m should return true if n has fewer than m divisors, 
 * (including 1 and n) and false otherwise:
few_divisors 17 3;;
- : bool = true
# few_divisors 4 3;;
- : bool = false
# few_divisors 4 4;;
- : bool = true
 *) 
(* The type signature for few_divisors is: *)
(* few_divisors : int -> int -> bool *)

let few_divisors (n : int) (m : int) : bool = 
  let rec checkDivisors (n : int) (currentIndex : int) : int =
    match currentIndex with 
      1 -> 1
      | _ -> if (n mod currentIndex) = 0 
        then checkDivisors (n) (currentIndex - 1) + 1 
        else checkDivisors (n) (currentIndex - 1)
    in
  if (checkDivisors n n) < m then true else false
;;

(* After writing few_divisors, uncomment the following lines to test your
 * code.  (Note: your code is not necessarily completely correct just because 
 * it passes these 3 tests.)  *)


assert(few_divisors 17 3);;
assert(not (few_divisors 4 3));;
assert(few_divisors 4 4);;


(* Problem 6 - Approximating Pi *)

let bad_arg (s:string) = failwith ("bad argument: "^s);;

(*>* Problem 6a *>*)
(* Sinusoidal Approximation: write the function sin_pi *)
(* sin_pi : int -> float *)
(* use the following equations to define a function that returns the ith
 * approximation of pi.  Call bad_arg if the argument i to sin_pi is less
 * than 0.

 * approx(0) = 3
 * approx(n+1) = approx(n) + sin(approx(n))

 * Using this approximation, you will converge on many digits of pi very
 * fast.  The first few digits of pi are 3.14159 26535 89793 23846 26433.  
 * Approximation 1 accurately predicts these digits:  3.141
 * Approximation 2 accurately predicts these digits:  3.14159 26535
 * Approximation 3 accurately predicts these digits:  3.14159 26535 89793
 * 
 *)

let sin_pi (i : int) : float = 
  if i < 0 then bad_arg (string_of_int i)
  else
  let rec nthApprox (i : int) : float = 
    match i with 
    0 -> 3.
    | _ -> nthApprox (i-1) +. sin(nthApprox(i-1))
  in
  nthApprox i
;;

(*>* Problem 6b *>*)
(* Monte Carlo Approximation: write the function monte_pi
 *
 * monte_pi : int -> float
 *
 * A Monte Carlo method relies on repeated random sampling to simulate
 * some process or compute a value.  See Wikipedia:
 * http://en.wikipedia.org/wiki/Monte_Carlo_method
 * 
 * Pi can be computed using Monte Carlo simulation through a series
 * of experiments.  Here is a single experiment:
 *
 *  -- choose a pair of random floating point numbers between 0 and 1
 *  -- call the numbers x and y 
 *  -- think of (x,y) as a point on the plane in the unit square
 *  -- test whether the point falls within the unit circle by measuring
 *     the distance from the point to the origin:  x^2 + y^2 <= 1
 *
 * Now suppose you do m experiments and in n of those experiments, the
 * random point chosen falls within the upper right quarter of the unit circle.
 * Since the area of a circle is known to be pi * r^2 and the area of
 * a square is r^2 (and here we are dealing with a radius/square side
 * of length 1), the following equations hold:

  n    quarter of area of circle     1/4 * pi * r^2
 --- = -------------------------  =  -------------- = 1/4 * pi
  m        area of square                r^2

 * Use the above information to write the function monte_pi, which 
 * takes a positive number indicating the number of random points n to
 * sample and approximates pi using that number of random points.
 * Call bad_arg when a non-positive argument is thrown.
 *
 * To compute some random numbers, use O'Camls Random library:
 *
 * http://caml.inria.fr/pub/docs/manual-ocaml/libref/Random.html
 * 
 * We initialize the library below.
 *
 * Random.float f will return a random floating point number between 0.0 and f.
 *
 * Note: this estimation method will converge far more slowly than the
 * sinusoidal method (because the sin function already captures pi, so
 * that approximation was really cheating!).  I only had the first 2 
 * digits after 5000 trials.
 * I estimated pi at 3.141628 after 1,000,000 trials.
 *)

Random.init 17;; (* Don't remove this line; Your code follows *)

let monte_pi (i : int) : float = 
  if i < 0 then bad_arg (string_of_int i)
  else
    let square n = n *. n in
    let rec count_successes (currentIndex : int) : int = 
      let (x, y) = (Random.float 1., Random.float 1.) in
      match currentIndex with 
        0 -> 0
      | _ -> if (square x +. square y <= 1.) 
        then (count_successes (currentIndex - 1) + 1) 
        else count_successes (currentIndex - 1)
    in 
  float_of_int (count_successes i) /. float_of_int (i) *. 4.
;;

(*************)
(* Problem 7 *)
(*************)

(* Look up another technique for approximating pi on the web.
 * As a starting point, see here:  
 *
 * http://en.wikipedia.org/wiki/Approximations_of_%CF%80
 *
 * You might be able to find other interesting articles on the web too.
 * 
 * The algorithm you choose must be capable of computing many digits of
 * pi.  Algorithms that compute just 1 approximation (such as 3 or
 * 3927/1250 or any other fixed fraction) are insufficient.  Choose 
 * an algorithm that successively approximates pi in some manner.  
 * Your algorithm may not use trigonometric functions such as sin, 
 * cos, arctan, etc.
 *
 *)

(* 7a:  Explain your algorithm and your sources here:

 I'm using the Madhava approximation from Wikipedia, where pi is 
 equal to sqrt(12) * sum_k_from_0_to_infinity ((-1/3)^k)/(2k + 1).

 *)

(* 7b:  Implement your algorithm here. *)
(*      Your algorithm should take a positive integer parameter 
 *      which increases the precision of your approximation as it 
 *      increases. Explain what the parameter is used for in your 
 *      algorithm and show some tests.
 *      The signature for your function is: custom_pi : int -> float
 *)

(* custom_pi: This function uses the Madhava approximation (shown above) where
 * "i" represents the sum of the first i terms of the infinite sum detailed
 * in question 7a.
 *
 * custom_pi testing:
 * 
 * Approximation  1 accurately predicts these digits:  3.
 * Approximation  5 accurately predicts these digits:  3.141
 * Approximation 10 accurately predicts these digits:  3.14159
 * Approximation 15 accurately predicts these digits:  3.14159 265
 * Approximation 20 accurately predicts these digits:  3.14159 26535
 * Approximation 25 accurately predicts these digits:  3.14159 26535 897
 * Approximation 30 accurately predicts these digits:  3.14159 26535 8979
 *
 *)

let custom_pi (i : int) : float = 
  if i < 0 then bad_arg (string_of_int i)
else
  let rec power (n : float) (k : int) : float = 
    match k with
    0 -> 1.
    | _ -> n *. (power n (k-1)) 
  in
  let rec nthApprox (i : int) : float = 
    match i with 
    0 -> 1.
    | _ -> (power (-1. /. 3.) i) /. ((2. *. float_of_int i) +. 1.) +. nthApprox (i-1)
  in
  sqrt(12.) *. nthApprox i
;;