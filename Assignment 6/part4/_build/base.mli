type base = A | T | C | G
type dna = base list

val eq : base -> base -> bool

exception NotBase

val to_char : base -> char

(* raise NotBase when char not one of A T C G *)
val from_char : char -> base   

(* an arbitrary total order over bases *)
(* 0 is equal; -1 is less than; 1 is greater than *)
val compare : base -> base -> int

(* raise NotBase when string contains a char that is not one of A T C G *)
val dna_from_string : string -> dna  

val dna_to_string : dna -> string

val longer_dna_of : dna -> dna -> dna
