open! Core

type 'a t = Same of 'a | Different_dimensions [@@deriving sexp]

include Monad.S with type 'a t := 'a t

val of_unequal_lengths : 'a List.Or_unequal_lengths.t -> 'a t
