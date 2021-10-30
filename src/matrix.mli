type t = Vector.t list [@@deriving sexp]

val create : int -> int -> init:(unit -> float) -> t

val product : t -> Vector.t -> Vector.t Or_diff.t
