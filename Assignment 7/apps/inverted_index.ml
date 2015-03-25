(* inverted_index computes an inverted index for the contents of
 * a given file. The filename is the given string.
 * The results are output to stdout. *)

open Util
open Sequence
open ParSeq

(* FIXME: CHANGE to PARSEQ *)
let mkindex (args : string ) : unit = 
  let docSeq =
    let docList = load_documents args in
    let docListLength = List.length docList in
    tabulate (fun i -> List.nth docList i) docListLength
  in
  (* Create a sequence of (word, id) tuples for all the words in doc i *)
  let tupleSeqGen i =
    let currDoc = nth docSeq i in
    let docID = singleton currDoc.id in
    let contentsStringList = split_words (currDoc.contents) in
    let contentsLength = List.length contentsStringList in
    let contentSeq = tabulate (fun i -> ((List.nth contentsStringList i), docID)) contentsLength
  in
  let compareStringTuple a b = 
    let (aWord, _) = a in
    let (bWord, _) = b in
    String.compare aWord bWord
  in
  let tupleSeq =
    flatten (tabulate (fun i -> sort compareStringTuple (tupleSeqGen i)) docListLength)
  in
  let seqTupleSeq = map (fun tuple -> 
    filter (fun a -> 
      let (aWord, _) = a in
      let (tupleWord, _) = tuple in
      a = tuple
    ) tupleSeq
  ) tupleSeq in
  let invertedIndex = map (fun i -> 
    reduce (fun tuple1 tuple2 -> 
      let (word, seq1) = tuple1 in
      let (word, seq2) = tuple2 in
      (word, append seq1 seq2) (empty ()) i
    )
  ) seqTupleSeq in
  let printFun a =
    let (word, id_seq) = a in
    Printf.printf "%s : " word;
    let printID id = Printf.printf "%d, " id in
    let _ = map printID id_seq in
    print_newline;
  in
  map printFun invertedIndex in
  ()
;;

