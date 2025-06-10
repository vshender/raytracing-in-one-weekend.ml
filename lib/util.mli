(** [eps] is the epsilon value for floating point comparisons. *)
val eps : float

(** [pi] is the value of pi. *)
val pi : float

(** [float_equal ?epsilon f1 f2] checks if two floats are equal with a given
    precision. *)
val float_equal : ?epsilon:float -> float -> float -> bool

(** [degrees_to_radians degrees] converts degrees to radians. *)
val degrees_to_radians : float -> float
