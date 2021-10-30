open! Core

module Monad_arg = struct
  type 'a t = Same of 'a | Different_dimensions [@@deriving sexp]

  let return x = Same x

  let bind t ~f =
    match t with Same x -> f x | Different_dimensions -> Different_dimensions

  let map = `Define_using_bind
end

include Monad_arg
include Monad.Make (Monad_arg)

let of_unequal_lengths = function
  | List.Or_unequal_lengths.Ok x -> Same x
  | Unequal_lengths -> Different_dimensions
