type scalar = float
type point = scalar * scalar
type vector = point

let s_plus x y = x +. y

let s_minus x y = x -. y

let s_times x y = x *. y

let s_divide x y = x /. y

let s_dist (a,b) (c,d) = 
  sqrt (((s_minus a c) ** 2.) +. ((s_minus b d) ** 2.))

let s_compare a b = 
  if (a < b) then -1 
  else if (a = b) then 0
  else 1

let s_to_string = string_of_float 

let v_plus (a,b) (c,d) = (s_plus a c, s_plus b d)

let distance (a,b) (c,d) = s_dist (a,b) (c,d)

let midpoint (a,b) (c,d) = (((s_plus a c) /. 2.), ((s_plus b d) /. 2.))

let head (a,b) = (a,b)

let sum f s = Sequence.ParSeq.map_reduce f (0., 0.) v_plus s

let scale_point s (x,y) = (s_times s x, s_times s y)

let unit_vector p1 p2 = 
  let (a, b) = p1 in
  let (c, d) = p2 in
  let (e, f) = (s_minus c a, s_minus d b) in
  scale_point (s_divide 1. (distance (e, f) (a, b))) (e, f)
