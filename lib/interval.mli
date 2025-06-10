(** The type of intervals. *)
type t = {
  min : float;  (** Minimum value. *)
  max : float;  (** Maximum value. *)
}

(** Create a new interval.  The default interval is empty. *)
val make : float -> float -> t

(** Get the size of the interval. *)
val size : t -> float

(** Check if a value is contained in the interval. *)
val contains : t -> float -> bool

(** Check if a value is surrounded by the interval. *)
val surrounds : t -> float -> bool

(** The empty interval.  Doesn't contain any values. *)
val empty : t

(** The universe interval.  Contains all possible values. *)
val universe : t
