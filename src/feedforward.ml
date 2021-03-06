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
  { weights = make_weights layers; biases; activation; activation' }

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

let train_exn t ~inputs ~outputs ~cost' ~step =
  List.fold2_exn inputs outputs ~init:(1, t) ~f:(fun (i, t) input output ->
      let cost' = cost' ~expected:output in
      let step = step ~i in
      let t, _ = run_and_backpropagate_exn t ~input ~cost' ~step in
      (i + 1, t))
  |> snd

let%expect_test "train" =
  let s x = 1. /. (1. +. Float.exp (-.x)) in
  let activation = Vector.map ~f:s in
  let activation' =
    Vector.map ~f:(fun v ->
        let v' = s v in
        v' *. (1. -. v'))
  in
  let input = Vector.of_list [ 0.2; 0.8 ] in
  let output = Vector.of_list [ 0.8; 0.2 ] in
  let cost' ~expected ~output =
    Vector.sum_exn output (Vector.scale expected ~k:(-1.))
  in
  let step ~i = 1. /. Float.sqrt (Float.of_int i) in
  let runtest layers n =
    let t = create layers ~init:(Fn.const 0.5) ~activation ~activation' in
    let inputs = List.init n ~f:(Fn.const input) in
    let outputs = List.init n ~f:(Fn.const output) in
    let t = train_exn t ~inputs ~outputs ~cost' ~step in
    let output = run_exn t ~input in
    print_s [%message (output : Vector.t)]
  in
  runtest [ 2; 2 ] 100;
  runtest [ 2; 4; 3; 2 ] 100;
  [%expect
    {|
    (output (0.77607278089025122 0.297703317678373))
    (output (0.818659281733572 0.24360293858429807))
    |}]
