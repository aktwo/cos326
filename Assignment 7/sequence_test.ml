open Sequence

let init_logger () =
  Logger.init();
  Random.self_init ()
;;

let testingLength = 100;;

(* This test is NOT fully functional to test ParSeq.cons but to
   demonstrate the usage of the logger module
*)

let test_length () = 
  let seq = ParSeq.tabulate (fun i -> i) testingLength in
  init_logger();
  let length = ParSeq.length seq in
  (Printf.printf "Work: %d, Span: %d\n"
    (Logger.get_work()) (Logger.get_span());
  assert (length = testingLength))
;;

let test_empty () = 
  init_logger();
  let seq = ParSeq.empty () in 
  Printf.printf "Work: %d, Span: %d\n"
    (Logger.get_work()) (Logger.get_span());
  assert ((ParSeq.length seq) = 0)
;;


let test_singleton () = 
  init_logger();
  let seq = ParSeq.singleton 1 in
  Printf.printf "Work: %d, Span: %d\n"
    (Logger.get_work()) (Logger.get_span());
  assert ((ParSeq.length seq) = 1);
  assert ((ParSeq.nth seq 0) = 1)
;;

let test_cons () =
  let seq = ParSeq.empty () in
  init_logger();
  let seq = ParSeq.cons 1 seq in
  Printf.printf "Work: %d, Span: %d\n"
    (Logger.get_work()) (Logger.get_span());
  assert (ParSeq.length seq = 1)
;;

let test_many_cons () =
  let lseq = ListSeq.empty() in
  let pseq = ParSeq.empty() in
  let rec add_list_item seq i =
    if (i <= 0) then seq else
    let seq = ListSeq.cons i seq in
    add_list_item seq (i-1)
  in
  let rec add_item seq i =
    if (i <= 0) then seq else
    let seq = ParSeq.cons i seq in
    add_item seq (i-1)
  in
  let lseq = add_list_item lseq testingLength in
  init_logger();
  let pseq = add_item pseq testingLength in
  Printf.printf "Work: %d, Span: %d\n"
    (Logger.get_work()) (Logger.get_span());
  assert ((ListSeq.length lseq) = testingLength);
  assert ((ParSeq.length pseq) = testingLength)
;;

let test_map () =
  let seq = ParSeq.repeat 1 testingLength in
  init_logger();
  let seq = ParSeq.map (fun y -> y+1) seq in
  Printf.printf "Work: %d, Span: %d\n" (Logger.get_work()) (Logger.get_span());
  let len = ParSeq.length seq in
  let rec helper i =
    if (i = len) then () else
    (assert ((ParSeq.nth seq i) = 2); helper (i+1))
  in
  helper 0
;;

let test_tabulate () = 
  init_logger();
  let tabulateSeq = ParSeq.tabulate (fun i -> i) testingLength in
  Printf.printf "Work: %d, Span: %d\n"
    (Logger.get_work()) (Logger.get_span());
  let rec check_tabulate tseq i length = 
    if (i = length) then () else
    (assert ((ParSeq.nth tseq i) = i);
    check_tabulate tseq (i+1) length)
  in
  check_tabulate tabulateSeq 0 testingLength
;;

let test_filter () = 
  let initialSeq = ParSeq.tabulate (fun i -> i) testingLength in
  init_logger();
  let filteredSeq = ParSeq.filter (fun i -> i > 50) initialSeq in
  Printf.printf "Work: %d, Span: %d\n"
    (Logger.get_work()) (Logger.get_span());
  let length = ParSeq.length filteredSeq in
  let rec check_filter seq i = 
    if (i = length) then () else
      (assert ((ParSeq.nth seq i) > 50);
      check_filter seq (i+1))
  in check_filter filteredSeq 0
;;

let test_append () =
  let seq1 = ParSeq.repeat 1 testingLength in
  let seq2 = ParSeq.repeat 2 testingLength in
  init_logger();
  let seq3 = ParSeq.append seq1 seq2 in
  Printf.printf "Work: %d, Span: %d\n"
    (Logger.get_work()) (Logger.get_span());
  let rec test i n =
    if (i = n) then () else
    (assert (ParSeq.nth seq3 i = (i/testingLength)+1);
      test (i+1) n)
  in test 0 (testingLength*2)
;;

let test_reduce () =
  let seq = ParSeq.repeat 1 testingLength in
  init_logger();
  let computedLength = ParSeq.reduce (fun a b -> a+b) 0 seq in
  Printf.printf "Work: %d, Span: %d\n" (Logger.get_work()) (Logger.get_span());
  assert(computedLength = testingLength)
;;

let test_flatten () = 
  let seq = ParSeq.tabulate (fun i -> ParSeq.singleton i) testingLength in
  init_logger(); 
  let flattenedSeq = ParSeq.flatten seq in
  Printf.printf "Work: %d, Span: %d\n"
    (Logger.get_work()) (Logger.get_span());
  let rec check_flatten inputSeq i = 
    if (i = testingLength) then () else
    (assert ((ParSeq.nth inputSeq i) = i);
    check_flatten inputSeq (i+1))
  in
  check_flatten flattenedSeq 0
;;

let test_repeat () =
  init_logger(); 
  let seq = ParSeq.repeat 1 testingLength in
  Printf.printf "Work: %d, Span: %d\n"
    (Logger.get_work()) (Logger.get_span());
  let rec check_repeat inputSeq i = 
    if (i = testingLength) then () else
    (assert ((ParSeq.nth inputSeq i) = 1);
    check_repeat inputSeq (i+1))
  in 
  check_repeat seq 0
;;

let test_zip () = 
  let seq1 = ParSeq.tabulate (fun i -> i) testingLength in
  let seq2 = ParSeq.tabulate (fun i -> i+100) testingLength in
  init_logger();
  let zippedSeq = ParSeq.zip (seq1, seq2) in
  Printf.printf "Work: %d, Span: %d\n"
    (Logger.get_work()) (Logger.get_span());
  let rec check_zip inputSeq i = 
    if (i = testingLength) then () else
    (assert ((ParSeq.nth inputSeq i) = (i, i+100));
    check_zip inputSeq (i+1))
  in 
  check_zip zippedSeq 0
;;

let test_split () = 
  let seq = ParSeq.tabulate (fun i -> i) testingLength in
  init_logger();
  let (left, right) = ParSeq.split seq (testingLength / 2) in
  Printf.printf "Work: %d, Span: %d\n"
    (Logger.get_work()) (Logger.get_span());
  let rec check_left i = 
    if (i = (testingLength/2)) then () else
    (assert ((ParSeq.nth left i) < (testingLength / 2));
    check_left (i+1))
  in 
  let rec check_right i = 
    if (i = (testingLength/2)) then () else
    (assert ((ParSeq.nth right i) >= (testingLength / 2));
    check_right (i+1))
  in
  let _ = (check_left 0, check_right 0) in
  ()
;;

(* more tests here *)

let run_tests() =
  Printf.printf "Testing length\n";
  test_length();
  Printf.printf "Testing empty\n";
  test_empty();
  Printf.printf "Testing singleton\n";
  test_singleton();
  Printf.printf "Testing cons\n";
  test_cons();
  Printf.printf "Testing map\n";
  test_map();
  Printf.printf "Testing tabulate\n";
  test_tabulate();
  Printf.printf "Testing filter\n";
  test_filter();
  Printf.printf "Testing append\n";
  test_append();
  Printf.printf "Testing reduce\n";
  test_reduce();
  Printf.printf "Testing flatten\n";
  test_flatten();
  Printf.printf "Testing repeat\n";
  test_repeat();
  Printf.printf "Testing zip\n";
  test_zip();
  Printf.printf "Testing split\n";
  test_split()
;;

run_tests()