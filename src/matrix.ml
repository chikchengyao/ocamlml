open! Core

type t = Vector.t list [@@deriving sexp]

let create x y ~init = List.init y ~f:(fun _ -> Vector.create x ~init)

let product matrix vector =
  List.map matrix ~f:(fun v -> Vector.dot vector v) |> Or_diff.all
