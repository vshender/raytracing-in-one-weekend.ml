(** The type of 3D points. *)
type t = {
  x : float;
  y : float;
  z : float;
}

(** [make x y z] creates a new point with the given coordinates *)
val make : float -> float -> float -> t

(** [move p ~by:v] is a new point obtained by moving [p] by the vector [v]. *)
val move : t -> by:Vec3.t -> t

(** [sub p1 p2] is the vector from [p2] to [p1]. *)
val sub : t -> t -> Vec3.t

(** Infix operators for point operations. *)
module Infix : sig
  (** [p + v] moves the point [p] by the vector [v]. *)
  val ( + ) : t -> Vec3.t -> t

  (** [p1 - p2] is the vector from [p2] to [p1]. *)
  val ( - ) : t -> t -> Vec3.t
end
