
let par (f:'a -> unit) (x:'a) (g:'b -> unit) (y:'b) : unit =
  let choice = Random.int 2 in
  let (t_work, t_span) = (ref 0, ref (-1)) in
  (
    if (choice = 0) then (
      let (w, s) = Logger.do_work (fun () -> f x) in
      t_work := !t_work + w; t_span := max !t_span s;
      let (w, s) = Logger.do_work (fun () -> g y) in
      t_work := !t_work + w; t_span := max !t_span s;
    )
    else (
      let (w, s) = Logger.do_work (fun () -> g y) in
      t_work := !t_work + w; t_span := max !t_span s;
      let (w, s) = Logger.do_work (fun () -> f x) in
      t_work := !t_work + w; t_span := max !t_span s
    )
  );
  Logger.update_total_work (!t_work + 1);
  Logger.update_total_span (!t_span + 1)


let parp (f:'a -> 'c) (x:'a) (g:'b -> 'd) (y:'b) : 'c * 'd =
  let fr = ref None in
  let gr = ref None in
  let fwork () = fr := Some (f x) in
  let gwork () = gr := Some (g y) in 
  let choice = Random.int 2 in
  let (t_work, t_span) = (ref 0, ref (-1)) in
  (
    if (choice = 0) then (
      let (w, s) = Logger.do_work fwork in
      t_work := !t_work + w; t_span := max !t_span s;
      let (w, s) = Logger.do_work gwork in
      t_work := !t_work + w; t_span := max !t_span s;
    )
    else (
      let (w, s) = Logger.do_work gwork in
      t_work := !t_work + w; t_span := max !t_span s;
      let (w, s) = Logger.do_work fwork in
      t_work := !t_work + w; t_span := max !t_span s
    )
  );
  Logger.update_total_work (!t_work + 1);
  Logger.update_total_span (!t_span + 1);
  (match !fr, !gr with
      Some v1, Some v2 -> (v1, v2)
    | _, _ -> failwith "impossible")

module WorkSet = Myset.Make(
  struct
    open Order
    type t = int
    let compare x y = if x < y then Less else if x > y then Greater else Eq
    let string_of_t = string_of_int
    let gen () = 0
    let gen_random =
      let _ = Random.self_init () in
      (fun () -> Random.int 10000)
    let gen_gt x () = x + 1
    let gen_lt x () = x - 1
    let gen_between x y () =
      let (lower, higher) = (min x y, max x y) in
      if higher - lower < 2 then None else Some (higher - 1)
  end)

(* Note:  Due to the sequential for loop, this implementation has a true
 * span of at least n.  We will disregard that fact as we are counting
 * array operations.  For our purposes, the span is:
 * max(span(f 0), span(f 1), ..., span(f (n-1))) + 1
 *)
let multi_par (f:int -> unit) (n:int) : unit =
  let set = ref WorkSet.empty in
  let (t_work, t_span) = (ref 0, ref (-1)) in
  for i=0 to n-1 do
    let choice = ref (Random.int n) in
    while (WorkSet.member !set !choice) do
      choice := Random.int n
    done;
    let (w, s) = Logger.do_work (fun () -> f !choice) in
    t_work := !t_work + w; t_span := max !t_span s;
    set := WorkSet.insert !choice !set
  done;
  Logger.update_total_work (!t_work + 1);
  Logger.update_total_span (!t_span + 1)
