
  type 'a t = 'a array

  exception Empty_list

  let length arr =
    Logger.incr_work ();
    Logger.incr_span ();
    Array.length arr

  let get arr i =
    let r = Array.get arr i in
    Logger.incr_work ();
    Logger.incr_span ();
    r

  let set arr i v =
    let arr = Array.set arr i v in
    Logger.incr_work ();
    Logger.incr_span ();
    arr

  let make n x =
    let arr = Array.make n x in
    Logger.add_to_work n;
    Logger.incr_span ();
    arr
