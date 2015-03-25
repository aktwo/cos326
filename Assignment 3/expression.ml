(* 

Name: Akshay Kumar
Email: aktwo@
Minutes Spent on Problem 2: ~6 hours

(You aren't in any way graded on the number of minutes spent; 
 we are just trying to calibrate for future versions of the class)

Comments/Problems/Thoughts on this part of the assignment:
This is SO COOL! I can't believe that we're doing this in the
fourth week :)
*)

open Ast ;;
open ExpressionLibrary ;;

(* TIPS FOR PROBLEM 2:
 * 1. Read the writeup.
 * 2. Use the type definitions in the ast.ml as a reference. But don't worry 
 *    about expressionlibrary.ml
 * 3. Test!  (Use "assert" where appropriate.)
 *)

(*>* Problem 2.1 *>*)

(* contains_var : tests whether an expression contains a variable "x"
 *     Examples : contains_var (parse "x^4") = true
 *                contains_var (parse "4+3") = false *)
let rec contains_var (e:expression) : bool =
  match e with
  | Num z -> false 
  | Var -> true
  | Binop (_, x, y) -> (contains_var x) || (contains_var y) 
  | Unop (_, x) -> contains_var x
;;

assert(contains_var (parse "x^4") = true);;
assert(contains_var (parse "4+3") = false);;

(*>* Problem 2.2 *>*)

(* evaluate : evaluates an expression for a particular value of x. Use OCaml's
 *            built in method of handling 'divide by zero' errors.
 *  Example : evaluate (parse "x^4 + 3") 2.0 = 19.0 *)
let rec evaluate (e:expression) (x:float) : float =
  let rec power (base: float) (exp: int) : float = 
    match exp with
    | 0 -> 1.
    | n -> base *. (power base (n-1))
  in
  match e with 
  | Num z -> z
  | Var -> x
  | Binop (op, a, b) -> (
    match op with 
    | Add -> (evaluate a x) +. (evaluate b x)
    | Sub -> (evaluate a x) -. (evaluate b x)
    | Mul -> (evaluate a x) *. (evaluate b x)
    | Div -> (evaluate a x) /. (evaluate b x)
    | Pow -> power (evaluate a x) (int_of_float (evaluate b x)))
  | Unop (op, a) -> (
    match op with 
    | Sin -> sin (evaluate a x)
    | Cos -> cos (evaluate a x)
    | Ln -> log (evaluate a x)
    | Neg -> -1. *. (evaluate a x))
;;

assert (evaluate (parse "x^4 + 3") 2.0 = 19.0);;

(*>* Problem 2.3 *>*)

(* See writeup for instructions.  *)
let rec derivative (e:expression) : expression =
  match e with 
  | Num x -> Num (0.)
  | Var -> Num (1.)
  | Binop (op, a, b) -> (
    match op with 
    | Add -> Binop (Add, (derivative a), (derivative b))
    | Sub -> Binop (Sub, (derivative a), (derivative b))
    | Mul -> Binop (Add, Binop (Mul, (derivative a), b), Binop (Mul, a, (derivative b)))
    | Div -> Binop (Div, Binop (Sub, Binop (Mul, b, (derivative a)), Binop (Mul, a, (derivative b))), Binop (Pow, b, Num (2.))) 
    | Pow -> (
      match b with
      | Num _ -> Binop (Mul, Binop (Mul, b, Binop (Pow, a, Binop (Sub, b, Num(1.)))), (derivative a))
      | Var | Binop _ | Unop _ -> Binop (Mul, Binop (Pow, a, b), Binop (Add, Binop (Mul, (derivative b), Unop(Ln, a)), Binop (Div, Binop (Mul, (derivative a), b), a)))))
  | Unop (op, a) -> (
    match op with
    | Sin -> Binop (Mul, (derivative a), Unop (Cos, a))
    | Cos -> Binop (Mul, (derivative a), Unop (Neg, Unop (Sin, a)))
    | Ln ->  Binop (Div, (derivative a), a)
    | Neg -> Unop (Neg, (derivative a)))
;;

(* A helpful function for testing. See the writeup. *)
let checkexp strs xval=
  print_string ("Checking expression: " ^ strs^"\n");
  let parsed = parse strs in (
        print_string "contains variable : ";
	print_string (string_of_bool (contains_var parsed));
	print_endline " ";
	print_string "Result of evaluation: ";
	print_float  (evaluate parsed xval);
	print_endline " ";
	print_string "Result of derivative: ";
	print_endline " ";
	print_string (to_string (derivative parsed));
	print_endline " ");;


(*>* Problem 2.4 *>*)

(* See writeup for instructions. *)
let rec find_zero (e:expression) (g:float) (epsilon:float) (lim:int) : float option =
  if (((evaluate e g) < epsilon) && ((evaluate e g) > (-1. *. epsilon)))
  then Some g
  else
    match (g, lim) with
    | (_, 0) -> Some g
    | (_, _) -> find_zero e (g -. ((evaluate e g) /. (evaluate (derivative e) g))) epsilon (lim - 1)
;;

(*>* Problem 2.5 *>*)
(* See writeup for instructions. *)
let rec find_zero_exact (e:expression) : expression option =
  
  (* Helper method to join two polynomials. If either of the lists is empty
   * then we know that one of the sub-expressions contains a prohibited operator
   * so the function returns an empty list *)
  let join_polynomials (list1 : (float * int) list) 
      (list2 : (float * int) list) : (float * int) list =
    match (list1, list2) with
    | ([], []) -> []
    | (xs, []) -> []
    | ([], xs) -> []
    | (xs, ys) -> xs @ ys
  in
  
  (* Helper method to take a curr_exp of type expression and 
   *  a polynomial to multiply (prev_poly) and returns a polynomial 
   *  in list form of form [(coefficient_1, power_1); (coefficient_2, power_2);...]
   *)
  let rec generate_polynomial 
      (curr_exp : expression) (prev_poly: (float * int) list) : (float * int) list = 
  match (curr_exp, prev_poly) with
  | (_, []) -> []
  | (Num a, _) -> List.map (fun (coeff, pow) -> (coeff *. a, pow)) prev_poly
  | (Var, _) -> List.map (fun (coeff, pow) -> (coeff, pow + 1)) prev_poly
  | (Binop (op, a, b), _) -> (
    match op with
    | Add -> join_polynomials (generate_polynomial a prev_poly) 
      (generate_polynomial b prev_poly)
    | Sub -> join_polynomials (generate_polynomial a prev_poly) 
      (generate_polynomial b (List.map (fun (coeff, pow) -> (-1. *. coeff, pow)) prev_poly))
    | Mul -> (generate_polynomial b (generate_polynomial a prev_poly))
    | Div | Pow -> []
  )
  | (Unop (op, a), _) -> (
    match op with
    | Neg -> 
      generate_polynomial a (List.map (fun (coeff, pow) -> (-1. *. coeff, pow)) prev_poly)
    | Cos | Sin | Ln -> []
  )
  in
  
  (* This function takes the output of the generate_polynomial function and collects coefficients
   * of terms with the same power. It returns a (coeff, pow) list representation of the resulting
   * polynomial. *)
  let rec collect_terms 
      (prev_list : (float * int) list) (new_element : float * int) : (float * int) list = 
    let (new_coeff, new_pow) = new_element in
    match prev_list with
    | [] -> [new_element]
    | (curr_coeff, curr_pow) :: tl -> (
      if (curr_pow = new_pow)
      then (curr_coeff +. new_coeff, curr_pow) :: tl
      else
	if (curr_coeff = 0.)
	then collect_terms tl new_element
	else (curr_coeff, curr_pow) :: collect_terms tl new_element
  )
  in
  
  (* This function takes the output of the collect_terms function 
   * and removes all zero-coefficient terms from the list *)
  let rec remove_zero_coefficients 
      (input : (float * int) list) : (float * int) list = 
    match input with
    | [] -> [] 
    | (curr_coeff, curr_pow) :: tl -> (
      if (curr_coeff = 0.)
      then remove_zero_coefficients tl
      else (curr_coeff, curr_pow) :: remove_zero_coefficients tl
    )
  in
  
  (* This function takes in the output of remove_zero_coefficients and returns
   * true if the equation is of order "order" and false otherwise *)
  let rec check_order (order : int) (input: (float * int) list) : bool = 
    match input with
    | [] -> true
    | (curr_coeff, curr_pow) :: tl -> (
      if (curr_pow > order)
      then false
      else check_order order tl
    )
  in
  
  (* This function takes the output of remove_zero_coefficients and, assuming
   * that the input polynomial list is of order 0 or order 1, returns 
   * an expression option representing the solution *)
  let solve (input : (float * int) list) : expression option = 
    match input with
    | [(b, 0); (a, 1)] -> Some (Num (-1. *. b /. a))
    | [(a, 1); (b, 0)] -> Some (Num (-1. *. b /. a))
    | [(a, 1);] -> Some (Num 0.)
    | [(b, 0);] -> None
    | [] -> None
    | _ -> None
  in
  match e with
  | Num x -> Some (Num x)
  | Var -> Some (Num 0.)
  | Binop (_, _, _) | Unop (_, _) -> (
    
    (* transform expression into polynomial list representation *)
    let polynomial = generate_polynomial e [(1., 0)] in
    match polynomial with
    | [] -> None
    | [(0., _)] -> None
    | x -> (
      
      (* collect terms and remove zero coefficient terms *)
      let simplified_poly = 
	remove_zero_coefficients (List.fold_left (collect_terms) [] x) in
      
      (* check the order of the resulting simplified polynomial list *)
      if (check_order 1 simplified_poly)
      
      (* solve the polynomial if appropriate *)
      then (solve simplified_poly)
      
      (* otherwise return None *)
      else None
    )
  )
;;

(* Quick and dirty testing code 
let test = parse "3*(x + 1) - x*(x-x) - 6";;
let first_result = generate_polynomial test [(1., 0)];;
let second_result = (List.fold_left (collect_terms) [] first_result);;
let third_result = remove_zero_coefficients second_result;;
let fourth_result = check_order 1 third_result;;
let fifth_result = solve third_result;;
*)

(*>* Problem 2.6 *>*)

(* Only adds parentheses when needed to prevent ambiguity. *)
(* See observations in the writeup. *)
let to_string_smart (e:expression) : string =
  let rec process (binop_parent : binop option) (e : expression) : string =  
    match e with
    | Num x -> string_of_float x
    | Var -> "x"
    | Binop (op, a, b) -> (
      match (binop_parent, op) with
      | (None, child_op) ->  (process (Some child_op) a) ^ (binop_to_string op) ^ (process (Some child_op) b)
      | (Some parent_op, child_op) -> (
	if (parent_op = child_op) && (binop_is_associative child_op)
	then (process (Some child_op) a) ^ (binop_to_string child_op) ^ (process (Some child_op) b)
	else
	  if (binop_precedence parent_op) > (binop_precedence child_op)
	  then (process (Some child_op) a) ^ (binop_to_string child_op) ^ (process (Some child_op) b)
	  else "(" ^ (process (Some child_op) a) ^ (binop_to_string child_op) ^ (process (Some child_op) b) ^ ")"
      )
    )
    | Unop (op, a) -> (unop_to_string op) ^ "(" ^ (process (None) (a)) ^ ")"
  in
  process (None) (e)
;;
