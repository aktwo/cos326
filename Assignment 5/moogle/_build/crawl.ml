open Util ;;    
open CrawlerServices ;;
open Order ;;
open Pagerank ;;


(* RandomWalkRanker and QuantumRanker are for karma questions only *)
module MoogleRanker
(* 
  = InDegreeRanker (PageGraph) (PageScore)
*)
  = RandomWalkRanker (PageGraph) (PageScore) (struct 
    let do_random_jumps = Some 0.20
    let num_steps = 1000
  end)

  (*  
   = QuantumRanker (PageGraph) (PageScore) (struct 
       let alpha = 0.01
      let num_steps = 1
       let debug = true
     end)
  *)

(* Dictionaries mapping words (strings) to sets of crawler links *)
module WordDict = Dict.Make(
  struct 
    type key = string
    type value = LinkSet.set
    let compare = string_compare
    let string_of_key = (fun s -> s)
    let string_of_value = LinkSet.string_of_set

    (* These functions are for testing purposes *)
    let gen_key () = ""
    let gen_key_gt x () = gen_key ()
    let gen_key_lt x () = gen_key ()
    let gen_key_random () = gen_key ()
    let gen_key_between x y () = None
    let gen_value () = LinkSet.empty
    let gen_pair () = (gen_key(),gen_value())
  end)

(* A query module that uses LinkSet and WordDict *)
module Q = Query.Query(
  struct
    module S = LinkSet
    module D = WordDict
  end)

let print s = 
  let _ = Printf.printf "%s\n" s in
  flush_all();;


(***********************************************************************)
(*    PART 1: CRAWLER                                                  *)
(***********************************************************************)

(* TODO: Build an index as follows:
 * 
 * Remove a link from the frontier (the set of links that have yet to
 * be visited), visit this link, add its outgoing links to the
 * frontier, and update the index so that all words on this page are
 * mapped to linksets containing this url.
 *
 * Keep crawling until we've
 * reached the maximum number of links (n) or the frontier is empty. *)
let rec crawl (n:int) (frontier: LinkSet.set)
    (visited : LinkSet.set) (d:WordDict.dict) : WordDict.dict = 
  
  if (n = 0)
  then d 
  else
    (* remove one element of the frontier *)
    match (LinkSet.choose frontier) with
    | None -> d 
    | Some (current_link, new_frontier) -> (
      (* check if that element is in visited set *)
      if (LinkSet.member visited current_link) 
      (* if yes, call crawl with n on new frontier *)
      then crawl n new_frontier visited d
      else (
	(* if no, visit it *)
	match (get_page current_link) with
	| None -> crawl n new_frontier visited d
	| Some page -> (
	  let rec insert_into_set (links: link list) (set: LinkSet.set) : LinkSet.set =
	    match links with
	    | [] -> set
	    | hd :: tl -> insert_into_set tl (LinkSet.insert hd set)
	  in
	  (* add all links to frontier using helper function *)
	  let updated_frontier = insert_into_set page.links new_frontier
	  in
	  let rec insert_into_dict (words: string list) (dict: WordDict.dict) : WordDict.dict =
	    match words with 
	    | [] -> dict
	    | words -> List.fold_left (fun dict word -> 
	      match WordDict.lookup dict word with
	      | None -> WordDict.insert dict word (LinkSet.insert current_link (LinkSet.empty))
	      | Some linkset -> WordDict.insert dict word (LinkSet.insert current_link linkset)
	    ) dict words
	  in
	  (* add all words into WordDict using helper function *)
	  let updated_dict = insert_into_dict page.words d
	  in
	  (* add all links to LinkSet *)
	  let updated_visited = LinkSet.insert current_link visited
	  in
	  (* call crawl with (n-1) on frontier *)
	  crawl (n-1) updated_frontier updated_visited updated_dict 
	)
      )
    )
;;

let crawler () =
  crawl num_pages_to_search (LinkSet.singleton initial_link) LinkSet.empty 
    WordDict.empty
;;

(* Debugging note: if you set debug=true in moogle.ml, it will print out your
 * index after crawling. *)
