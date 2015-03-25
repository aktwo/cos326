open Common_nbody

module Seq = Sequence.ParSeq

(* Representation of a 2d rectangular plane *)
type bbox = { north_west:Plane.point; south_east:Plane.point }

(* Our representation of a Barnes-Hut tree. You are free to use this type
 * or create your own. *)
type bhtree =
    Empty
  | Single of body
  | Cell of body * bbox * (bhtree Seq.t)

(* [quarters box]: Divide a bounding box into 4 equally sized quadrants. *)
let quarters (box:bbox) : bbox Seq.t =
  failwith "not implemented"

(* [psuedobody bodies]: Return a psuedobody representing all bodies in [bodies]  *)
let pseudobody (bodies:body Seq.t) : body =
  failwith "not implemented"

(* [make_bhtree bodies box]: Create Barnes-Hut tree of [bodies].
 * Raise an exception if a body in [bodies] is not contained within [box]. *)
let make_bhtree (bodies:body Seq.t) (box:bbox) : bhtree =
  failwith "Could not make tree because compute_tree is not implemented."

(* [in_range b1 b2]: returns true if the mass-weighted distance from
 * [b1] to [b2] is less than [theta]. Use to determine whether Barnes-Hut
 * approximation is appropriate for a body and psuedobody. *)
let in_range (b1:body) (b2:body) (theta:Plane.scalar) : bool =
  failwith "not implemented"

(* [acceleration tree theta body]: use [tree] and [theta] to approximate the
 * acceleration on [body] due to every other body in the tree.*)
let acceleration (tree:bhtree) (theta:Plane.scalar) (body:body) : Plane.vector =
  failwith "acceleration = ERROR"

(* [accelerations bodies theta box]: Approximate the acceleration vector for each
 * body in [bodies]. *)
let accelerations (bodies:body Seq.t) (theta:Plane.scalar) (box:bbox)
    : Plane.vector Seq.t =
  failwith "not implemented"

(* [update bodies theta]: update the positions of all bodies in [bodies] via
 * Barnes-Hut approximation *)
let update (bodies:body Seq.t) (theta:Plane.scalar) : body Seq.t =
  failwith "not implemented"

(* [make_simulation bodies theta]: create a stream representing the simulation. *)
let rec make_simulation (bodies:body Seq.t) (theta:Plane.scalar) : simulation =
  failwith "nuuuuuu"
