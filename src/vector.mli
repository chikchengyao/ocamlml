open! Core

type t = float list [@@deriving sexp]

include Stringable.S with type t := t

val create : int -> init:(unit -> float) -> t

val sum : t -> t -> t Or_diff.t

val hadamard : t -> t -> t Or_diff.t

val dot : t -> t -> float Or_diff.t
