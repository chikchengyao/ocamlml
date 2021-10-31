open! Core

module Or_diff = struct
  module Monad_arg = struct
    type 'a t = Same of 'a | Different_dimensions [@@deriving sexp]

    let return x = Same x

    let bind t ~f =
      match t with
      | Same x -> f x
      | Different_dimensions -> Different_dimensions

    let map = `Define_using_bind
  end

  include Monad_arg
  include Monad.Make (Monad_arg)

  let of_unequal_lengths = function
    | List.Or_unequal_lengths.Ok x -> Same x
    | Unequal_lengths -> Different_dimensions
end

module Vector = struct
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

  module For_testing = struct
    let of_list x = x
  end
end

module Matrix = struct
  type t = Vector.t list [@@deriving sexp]

  let create x y ~init = List.init y ~f:(fun _ -> Vector.create x ~init)

  let product matrix vector =
    List.map matrix ~f:(fun v -> Vector.dot vector v) |> Or_diff.all

  let transpose matrix =
    match matrix with
    | [] -> []
    | hd :: tl ->
        List.fold tl
          ~init:(List.map hd ~f:(fun x -> [ x ]))
          ~f:(fun acc vec ->
            List.map2_exn acc vec ~f:(fun acc_i vec_i -> vec_i :: acc_i))
        |> List.map ~f:List.rev

  let%expect_test "transpose simple" =
    let m = [ [ 0.; 1.; 2.; 3. ]; [ 4.; 5.; 6.; 7. ]; [ 8.; 9.; 10.; 11. ] ] in
    let tm = transpose m in
    print_s [%message (tm : t)];
    [%expect "(tm ((0 4 8) (1 5 9) (2 6 10) (3 7 11)))"]
end
