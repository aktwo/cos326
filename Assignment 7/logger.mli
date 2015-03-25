
(* Initializes the logging module *)
val init : unit -> unit

(* Does new work and returns the (work, span) for that work *)
val do_work : (unit -> unit) -> (int * int)

(* increments the work by 1 *)
val incr_work : unit -> unit

(* increments the span by 1 *)
val incr_span : unit -> unit

(* increments the work by n *)
val add_to_work : int -> unit

(* gets the total work logged till now *)
val get_work : unit -> int

(* gets the total span logged till now *)
val get_span : unit -> int

(* updates the total work with the incr provided *)
val update_total_work : int -> unit

(* updates the total span with the incr provided *)
val update_total_span : int -> unit
