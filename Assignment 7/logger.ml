
module WorkDict = Dict.Make(
  struct
    open Order
    type key = int (* work_id *)
    type value = int * int (* work, span *)
    let compare = Order.int_compare
    let string_of_key x = string_of_int x
    let string_of_value v =
      match v with (work, span) -> (string_of_int work)
	^ ":" ^ (string_of_int span)
    let gen_key () = 0
    let gen_key_gt x () = x + 1
    let gen_key_lt x () = x - 1
    let gen_key_between x y () =
      let (lower, higher) = (min x y, max x y) in
      if higher - lower < 2 then None else Some (higher - 1)
    let gen_key_random () = 0
    let gen_value () = (0, 0)
    let gen_pair () = (gen_key_random(), gen_value())
  end)

let is_init : bool ref = ref false
let root_work_id = 0
let wdict : WorkDict.dict ref = ref WorkDict.empty
let work_id : int ref = ref (-1)
let current_work_id : int ref = ref (-1)

let total_work : int ref = ref 0
let total_span : int ref = ref 0

let is_root_work () = (!current_work_id = root_work_id)

let init () =
  is_init := true;
  wdict := WorkDict.empty;
  (* root worker setup *)
  work_id := root_work_id;
  current_work_id := root_work_id;
  wdict := WorkDict.insert !wdict !current_work_id (0, 0);

  total_work := 0;
  total_span := 0

let verify_init() =
  if !is_init then ()
  else init()

let get_current_work_span () =
  verify_init();
  match WorkDict.lookup !wdict !current_work_id with
  | None -> failwith "Invalid reference to key in dictionary"
  | Some (w, s) -> (w, s)

let do_work (f : unit -> unit) : (int * int) =
  verify_init();
  let parent_work_id = !current_work_id in
  work_id := !work_id + 1;
  current_work_id := !work_id;
  wdict := WorkDict.insert !wdict !current_work_id (0, 0);
  f ();
  let (w, s) = get_current_work_span() in
  current_work_id := parent_work_id;
  (w, s)

let add_to_work (n : int) =
  verify_init();
  let (w, s) = get_current_work_span() in
  (* for root process, we can directly update the total work *)
  if is_root_work()
  then total_work := !total_work + n
  else wdict := WorkDict.insert !wdict !current_work_id (w + n, s)

let incr_work () = add_to_work 1

let incr_span () =
  verify_init();
  let (w, s) = get_current_work_span() in
  (* for root process, we can directly update the total span *)
  if is_root_work()
  then total_span := !total_span + 1
  else wdict := WorkDict.insert !wdict !current_work_id (w, s + 1)

let update_total_work (incr : int) =
  verify_init();
  (* there is a pending par above *)
  if not (is_root_work())
  then (let (w, s) = get_current_work_span() in
	wdict := WorkDict.insert !wdict !current_work_id (w + incr, s))
  (* there is no pending par above, so update totals *)
  else total_work := !total_work + incr

let update_total_span (incr : int) =
  verify_init();
  (* there is a pending par above *)
  if not (is_root_work())
  then (let (w, s) = get_current_work_span() in
	wdict := WorkDict.insert !wdict !current_work_id (w, s + incr))
  (* there is no pending par above, so update totals *)
  else total_span := !total_span + incr

(* Total Work done*)
let get_work () = !total_work

(* Total Span  *)
let get_span () = !total_span
