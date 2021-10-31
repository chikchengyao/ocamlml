open! Core

module Vector : sig
  type t [@@deriving sexp]

  include Stringable.S with type t := t

  val create : int -> init:(unit -> float) -> t

  val map : t -> f:(float -> float) -> t

  val sum_exn : t -> t -> t

  val hadamard_exn : t -> t -> t

  val dot_exn : t -> t -> float

  val scale : t -> k:float -> t

  val of_list : float list -> t
end

module Matrix : sig
  type t [@@deriving sexp]

  val create : int -> int -> init:(unit -> float) -> t

  val scale : t -> k:float -> t

  val sum_exn : t -> t -> t

  val product_exn : t -> Vector.t -> Vector.t

  val transpose : t -> t

  val v_times_vT : Vector.t -> Vector.t -> t
end
