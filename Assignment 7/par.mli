(* par f x g y:  
 * execute f x and g y in parallel; wait until both complete
 * work = work(f x) + work(g x) + 1
 * span = max(span(f x), span(g x)) + 1
 *)
val par : ('a -> unit) -> 'a -> ('b -> unit) -> 'b -> unit

(* like par, except returns a pair of results *)
val parp : ('a -> 'c) -> 'a -> ('b -> 'd) -> 'b -> 'c * 'd

(* multi_par f n:
 * execute f 0, f 1, ..., f (n-1) in parallel; wait until all complete 
 * work = work(f 0) + work(f 1) + ... + work(f (n-1)) + 1
 * span = max(span(f 0), span(f 1), ..., span(f (n-1))) + 1 
 *)
val multi_par : (int -> unit) -> int -> unit
