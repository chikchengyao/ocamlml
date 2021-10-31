open! Core
open Math

type t = {
  weights : Matrix.t list;
  biases : Vector.t list;
  activation : Vector.t -> Vector.t; [@sexp_opaque]
  activation' : Vector.t -> Vector.t; [@sexp_opaque]
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
  {
    weights = make_weights layers;
    biases;
    activation = Vector.map ~f:activation;
    activation' = Vector.map ~f:activation';
  }

let run_exn t ~input =
  List.fold2_exn t.weights t.biases ~init:input ~f:(fun acc weight bias ->
      let acc = acc in
      let a = Matrix.product_exn weight acc in
      let z = Vector.sum_exn a bias in
      t.activation z)

let%expect_test "basic run" =
  let t =
    create [ 2; 2 ] ~init:(fun () -> 0.5) ~activation:Fn.id ~activation':Fn.id
  in
  let res = run_exn t ~input:(Vector.of_list [ 0.5; 0.5 ]) in
  print_s [%message (res : Vector.t)];
  [%expect "(res (1 1))"]

let run_and_backpropagate_exn t ~input ~cost' ~step =
  (* Terminology from http://neuralnetworksanddeeplearning.com/chap2.html *)
  let activations_L_to_1, weighted_inputs_L_to_2 =
    List.fold2_exn t.weights t.biases
      ~init:([ input ], [])
      ~f:(fun acc weight bias ->
        let activations, weighted_inputs = acc in
        let prev_activation = List.hd_exn activations in
        let w_times_a = Matrix.product_exn weight prev_activation in
        let weighted_input = Vector.sum_exn w_times_a bias in
        let next_activation = t.activation weighted_input in
        (next_activation :: activations, weighted_input :: weighted_inputs))
  in
  let output = List.hd_exn activations_L_to_1 in
  let errors_2_to_L =
    match weighted_inputs_L_to_2 with
    | [] -> raise_s [%message "Too few layers to backpropagate"]
    | output_weighted_input :: weighted_inputs_L_minus_1_to_2 ->
        let output_error =
          Vector.hadamard_exn (cost' ~output)
            (t.activation' output_weighted_input)
        in
        let weights_L_to_3 = List.drop t.weights 1 |> List.rev in
        List.fold2_exn weights_L_to_3 weighted_inputs_L_minus_1_to_2
          ~init:[ output_error ] ~f:(fun acc weight weighted_input ->
            let errors = acc in
            let prev_error = List.hd_exn errors in
            let weight_T = Matrix.transpose weight in
            let weights_applied_to_errors =
              Matrix.product_exn weight_T prev_error
            in
            let next_error =
              Vector.hadamard_exn weights_applied_to_errors
                (t.activation' weighted_input)
            in
            next_error :: errors)
  in
  let biases =
    List.map2_exn t.biases errors_2_to_L ~f:(fun bias error ->
        let delta = Vector.scale error ~k:(-.step) in
        Vector.sum_exn bias delta)
  in
  let activations_1_to_L_minus_1 = List.drop activations_L_to_1 1 |> List.rev in
  let weights =
    let deltas =
      List.map2_exn activations_1_to_L_minus_1 errors_2_to_L
        ~f:Matrix.v_times_vT
      |> List.map ~f:(Matrix.scale ~k:(-.step))
    in
    List.map2_exn t.weights deltas ~f:Matrix.sum_exn
  in
  ({ t with biases; weights }, output)

let%expect_test "backpropagate iterated 2:2" =
  let sigma x = 1. /. (1. +. Float.exp (-.x)) in
  let sigma' x =
    let s = sigma x in
    s *. (1. -. s)
  in
  let input = Vector.of_list [ 0.2; 0.8 ] in

  let cost' ~output =
    let target = Vector.of_list [ 0.8; 0.2 ] in
    let negative_target = Vector.scale target ~k:(-1.) in
    Vector.sum_exn output negative_target
  in

  let runtest t max =
    let rec iter t n =
      if n > max then t
      else
        let step = 1. /. Float.sqrt (Float.of_int n) in
        let t, _ = run_and_backpropagate_exn t ~input ~cost' ~step in
        iter t (n + 1)
    in
    iter t 1
  in
  let t1 =
    create [ 2; 2 ] ~init:(fun () -> 0.5) ~activation:sigma ~activation':sigma'
  in
  let t1' = runtest t1 100000 in
  let t1'_out = run_exn t1' ~input in
  print_s [%message (t1'_out : Vector.t)];
  [%expect "(t1'_out (0.79999999999989224 0.20000000000021195))"];
  let t2 =
    create [ 2; 4; 3; 2 ]
      ~init:(fun () -> 0.5)
      ~activation:sigma ~activation':sigma'
  in
  let t2' = runtest t2 1000 in
  let t2'_out = run_exn t2' ~input in
  print_s [%message (t2'_out : Vector.t)];
  [%expect "(t2'_out (0.80057834665312355 0.20087002842648508))"]
