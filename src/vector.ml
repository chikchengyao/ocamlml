open! Core

module T = struct
  type t = float list [@@deriving sexp]
end

include T
include Sexpable.To_stringable (T)

let create x ~init = List.init x ~f:(fun _ -> init ())

let combine t1 t2 ~f =
  List.fold2 t1 t2 ~init:[] ~f:(fun acc x1 x2 -> f x1 x2 :: acc)
  |> Or_diff.of_unequal_lengths

let sum = combine ~f:( +. )

let hadamard = combine ~f:( *. )

let dot t1 t2 =
  List.fold2 t1 t2 ~init:0. ~f:(fun acc x1 x2 -> acc +. (x1 *. x2))
  |> Or_diff.of_unequal_lengths

let map = List.map
