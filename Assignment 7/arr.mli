  type 'a t

  (* Work = O(1), Span = O(1) *)
  val length : 'a t -> int

  (* Work = O(1), Span = O(1) *)
  val get : 'a t -> int -> 'a

  (* Work = O(1), Span = O(1) *)
  val set : 'a t -> int -> 'a -> unit

  (* Work = O(n), Span = O(1) *)
  val make : int -> 'a -> 'a t
