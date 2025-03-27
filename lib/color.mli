(** The type of colors. *)
type t = {
  r : float;  (** red component between 0.0 and 1.0. *)
  g : float;  (** green component between 0.0 and 1.0. *)
  b : float;  (** blue component between 0.0 and 1.0. *)
}

(** [create r g b] creates a new color from RGB components. *)
val make : float -> float -> float -> t

(** [of_vec3 v] creates a new color using the components of the vector [v]. *)
val of_vec3 : Vec3.t -> t

(** [output oc c] writes the color [c] to the output channel [oc]. *)
val output : out_channel -> t -> unit

(** [add c1 c2] sums the components of the colors [c1] and [c2]. *)
val add : t -> t -> t

(** [scale c ~by:s] scales the components of the color [c] by the scalar [s]. *)
val scale : t -> by:float -> t

(** Infix operators for color operations. *)
module Infix : sig
  (** [c1 + c2] is the sum of the colors [c1] and [c2]. *)
  val ( + ) : t -> t -> t

  (** [c * s] scales the components of the color [c] by the scalar [s]. *)
  val ( * ) : t -> float -> t

  (** [c / s] divides the components of the color [c] by the scalar [s]. *)
  val ( / ) : t -> float -> t
end
