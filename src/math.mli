open! Core

module Or_diff : sig
  type 'a t = Same of 'a | Different_dimensions [@@deriving sexp]

  include Monad.S with type 'a t := 'a t

  val of_unequal_lengths : 'a List.Or_unequal_lengths.t -> 'a t
end

module Vector : sig
  type t [@@deriving sexp]

  include Stringable.S with type t := t

  val create : int -> init:(unit -> float) -> t

  val map : t -> f:(float -> float) -> t

  val sum : t -> t -> t Or_diff.t

  val hadamard : t -> t -> t Or_diff.t

  val dot : t -> t -> float Or_diff.t

  module For_testing : sig
    val of_list : float list -> t
  end
end

module Matrix : sig
  type t [@@deriving sexp]

  val create : int -> int -> init:(unit -> float) -> t

  val product : t -> Vector.t -> Vector.t Or_diff.t

  val transpose : t -> t
end
