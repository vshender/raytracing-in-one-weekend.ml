(** The type of 3D vectors. *)
type t = {
  x : float;
  y : float;
  z : float;
}

(** [make x y z] creates a new 3D vector with the given components. *)
val make : float -> float -> float -> t

(** [length_squared v] is the squared length of the vector [v]. *)
val length_squared : t -> float

(** [length v] is the length (magnitude) of the vector [v]. *)
val length : t -> float

(** [neg v] is the negation of the vector [v]. *)
val neg : t -> t

(** [add v1 v2] is the vector sum of [v1] and [v2]. *)
val add : t -> t -> t

(** [sub v1 v2] is the vector difference of [v1] and [v2]. *)
val sub : t -> t -> t

(** [scale v ~by:s] is the vector [v] scaled by factor [s]. *)
val scale : t -> by:float -> t

(** [mul v1 v2] is the component-wise multiplication of [v1] and [v2]. *)
val mul : t -> t -> t

(** [div v ~by:s] is the vector [v] divided by scalar [s]. *)
val div : t -> by:float -> t

(** [dot v1 v2] is the dot product of vectors [v1] and [v2]. *)
val dot : t -> t -> float

(** [cross v1 v2] is the cross product of vectors [v1] and [v2]. *)
val cross : t -> t -> t

(** [normalize v] is a unit vector in the direction of [v]. *)
val normalize : t -> t

(** Infix operators for vector operations. *)
module Infix : sig
  (** [~-v] negates the vector [v]. *)
  val ( ~- ) : t -> t

  (** [v1 + v2] adds the vectors [v1] and [v2]. *)
  val ( + ) : t -> t -> t

  (** [v1 - v2] subtracts the vector [v2] from the vector [v1]. *)
  val ( - ) : t -> t -> t

  (** [v1 * v2] performs component-wise multiplication of the vectors [v1] and [v2]. *)
  val ( * ) : t -> t -> t

  (** [v / s] divides the vector [v] by the scalar [s]. *)
  val ( / ) : t -> float -> t

  (** [v1 *. v2] calculates the dot product of the vectors [v1] and [v2]. *)
  val ( *. ) : t -> t -> float

  (** [v1 ** v2] calculates the cross product of the vectors [v1] and [v2]. *)
  val ( ** ) : t -> t -> t
end
