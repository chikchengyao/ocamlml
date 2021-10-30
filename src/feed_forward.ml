open! Core

type t = {
  layers : int list;
  weights : Matrix.t list;
  biases : Vector.t list;
  activation : float -> float; [@sexp_opaque]
  activation' : float -> float; [@sexp_opaque]
}
[@@deriving sexp_of]

let create layers ~init ~activation ~activation' =
  let rec make_weights layers =
    match layers with
    | [] -> raise_s [%message "Neural network must have at least 1 layer"]
    | [ _ ] -> []
    | x :: y :: tl -> Matrix.create x y ~init :: make_weights (y :: tl)
  in
  let biases =
    List.map (List.drop layers 1) ~f:(fun n -> Vector.create n ~init)
  in
  { layers; weights = make_weights layers; biases; activation; activation' }

let run t ~input =
  List.fold2 t.weights t.biases ~init:(Or_diff.return input)
    ~f:(fun acc weight bias ->
      let open Or_diff.Let_syntax in
      let%bind acc = acc in
      let%bind a = Matrix.product weight acc in
      Vector.sum a bias)
  |> Or_diff.of_unequal_lengths |> Or_diff.join

let%expect_test "basic run" =
  let t =
    create [ 2; 2 ] ~init:(fun () -> 0.5) ~activation:Fn.id ~activation':Fn.id
  in
  let res = run t ~input:[ 0.5; 0.5 ] in
  print_s [%message (res : Vector.t Or_diff.t)];
  [%expect "(res (Same (1 1)))"]
