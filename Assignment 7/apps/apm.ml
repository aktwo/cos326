
open Util
open Sequence
open ParSeq

type profile = {
  firstname : string;
  lastname : string;
  sex : string;
  age : int;
  lo_agepref : int;
  hi_agepref : int;
  profession : string;
  has_children : bool;
  wants_children : bool;
  leisure : string;
  drinks : bool;
  smokes : bool;
  music : string;
  orientation : string;
  build : string;
  height : string
}

let convert (p : string) : profile =
  let s = String.concat " " (Str.split (Str.regexp_string "@") p) in
  Scanf.sscanf s "%s@ %s@ %s@ %d %d %d %s@ %B %B %s@ %B %B %s@ %s@ %s@ %s"
  (fun firstname lastname sex age lo_agepref hi_agepref profession has_children
       wants_children leisure drinks smokes music orientation build height ->
   { firstname = firstname;
     lastname = lastname;
     sex = sex;
     age = age;
     lo_agepref = lo_agepref;
     hi_agepref = hi_agepref;
     profession = profession;
     has_children = has_children;
     wants_children = wants_children;
     leisure = leisure;
     drinks = drinks;
     smokes = smokes;
     music = music;
     orientation = orientation;
     build = build;
     height = height
   })

let print_profile ({
     firstname = firstname;
     lastname = lastname;
     sex = sex;
     age = age;
     lo_agepref = lo_agepref;
     hi_agepref = hi_agepref;
     profession = profession;
     has_children = has_children;
     wants_children = wants_children;
     leisure = leisure;
     drinks = drinks;
     smokes = smokes;
     music = music;
     orientation = orientation;
     build = build;
     height = height } : profile) : unit =
  Printf.printf "%s %s\n" firstname lastname;
  Printf.printf "  sex: %s  age: %d  profession: %s\n" sex age profession;
  Printf.printf "  %s  %s\n" (if drinks then "social drinker" else "nondrinker") (if smokes then "smoker" else "nonsmoker");
  Printf.printf "  %s  %s\n"
    (if has_children then "has children" else "no children")
    (if wants_children then "wants children" else "does not want children");
  Printf.printf "  prefers a %s partner between the ages of %d and %d\n"
    (if (orientation="straight" && sex="F") || (orientation = "gay/lesbian" && sex="M") then "male" else "female")
    lo_agepref hi_agepref;
  Printf.printf "  likes %s music and %s\n" music leisure


let print_matches (n : string) ((p, ps) : profile * (float * profile) list) : unit =
  print_string "------------------------------\nClient: ";
  print_profile p;
  Printf.printf "\n%s best matches:\n" n;
  List.iter (fun (bci, profile) ->
    Printf.printf "------------------------------\nCompatibility index: %f\n" bci; print_profile profile) ps;
  print_endline ""

(* apm computes the potential love of your life.  The filename of a file
 * containing candidate profiles is in location 0 of the given array.
 * The number of matches desired is in location 1.  The first and last name
 * of the (human) client are in location 2 and 3, respectively.  The client's
 * profile must be in the profiles file.  Results are output to stdout. *)
let matchme (args : string array) : unit =
  let profileStringLi st =
    let f = open_in args[0] in
    let rec next accum =
      match (try Some (input_line f) with End_of_file -> None) with
      | None -> accum
      | Some line -> line :: accum
    in
    let psList = next [] in
    close_in f;
    psList
  in
  let pslsLen = list.Length profileStringList in
  let profileSeq =
    tabulate (fun i -> convert (List.nth profileStringList i)) pslsLen
  in
  (* Note that in the case where which a client name is given that has multiple
   * profiles associated with it, we choose the first one *)
  let client = nth (filter
    (fun prof -> ((prof.firstname = args[2]) && (prof.lastname = args[3])))
    profileSeq) 0
  in
  (*
   * Computes the compatibility between profiles c (client) and o (other).
   * Will return 0 if age preferences or orientations don't match.
   * Otherwise, the score is a normalized score representing how well the rest
   * of the profile features match up. It gives 1 point to each matched feature
   * and then divides the score by the total number of points possible *)
  let compatibility c o =
    let agepref p1 p2 =
      ((p1.lo_agepref <= p2.age) & (p1.hi_agepref >= p2.age))
    in
    let orient p1 p2 =
      if ((String.compare p1.orientation p2.orientation) <> 0) then false
      else if (
        match p1.orientation with
        | "straight" -> (String.compare p1.sex p2.sex) = 0 then false 
        | "gay/lesbian" -> (String.compare p1.sex p2.sex) <> 0 then true
        | _ -> failwith "Invalid input"
      )
      then false
      else true
    in
    if (not (agepref c o)) || (not (agepref o c)) then 0.
    else if (not (orient c o) || (not (orient o c))) then 0.
    else (
      let kids =
        let b1 = (c.wants_children = o.wants_children) in
        let b2 = (c.wants_children = o.has_children) in
        let b3 = (o.wants_children = c.has_children) in
        (int_of_bool b1) + (int_of_bool b2) + (int_of_bool b3)
      in
      let leisure = (int_of_bool ((String_compare c.leisure o.leisure) = 0)) in
      let drinks = (int_of_bool (c.drinks = o.drinks)) in
      let smokes = (int_of_bool (c.smokes = o.smokes)) in
      let music = (int_of_bool ((String_compare c.music o.music) = 0)) in
      let intScore = kids + leisure + drinks + smokes + music in
      (float_of_int intScore) /. 7
    )
  in
  let compSeq = map (fun p -> ((compatibility client p), p)) profileSeq in
  (* Arbitrarily sets tuple2 to be of higher value if the two compatibility values are equal.
   * IS THAT OKAY????? *)
  let comparator tuple1 tuple2 =
    let (c1,p1) = tuple1 in
    let (c2,p2) = tuple2 in
    if (c1 > c2) then 1
    else (-1)
  in
  let (matches, _) = split (sort compSeq) args[1] in
  print_matches (string_of_int args[0]) (client, matches)
;;

(*
WE NEED TO CHECK THAT WE AREN'T RETURNING PEOPLE AS BEING MOST COMPATIBLE WITH THEMSELVES
*)
