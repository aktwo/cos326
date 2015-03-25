open Common_nbody
open Plane

module Seq = Sequence.ParSeq

(* [acceleration b1 b2] : compute gravitational acceleration vector for [b1]
 * due to [b2]. *)
let acceleration (b1:body) (b2:body) : Plane.vector =
  let g = 6.67384e-11 in
  let (mass1, loc1, vel1) = b1 in
  let (mass2, loc2, vel2) = b2 in
  let scalarAcc = s_divide (s_times g mass2) (s_times (distance loc1 loc2) (distance loc1 loc2)) in
  scale_point scalarAcc (unit_vector loc1 loc2)

(* [accelerations bodies]: compute gravitational acceleration vector for
 *  each body in [bodies] *)
let accelerations (bodies:body Seq.t) : Plane.vector Seq.t =
  Seq.map (fun body -> Plane.sum (acceleration body) bodies) bodies

(* [update bodies]: apply acceleration to update the positions & velocities
 * of all bodies in [bodies] *)
let update (bodies:body Seq.t) : body Seq.t =
  let bodyAccelerations = accelerations bodies in
  Seq.tabulate (fun i -> 
    let (mass, loc, vel) = Seq.nth bodies i in
    let acc = Seq.nth bodyAccelerations i in
    (mass, v_plus (v_plus loc vel) (scale_point (1. /. 2.) acc), v_plus vel acc)
  ) (Seq.length bodies)

(* [make_simulation bodies]: Create a stream representation of the N-Body simulation  *)
let rec make_simulation (bodies:body Seq.t) : simulation =
  Cons(bodies, fun () -> make_simulation (update bodies))