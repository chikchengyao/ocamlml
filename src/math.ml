open! Core

module Vector = struct
  module T = struct
    type t = float list [@@deriving sexp]
  end

  include T
  include Sexpable.To_stringable (T)

  let create x ~init = List.init x ~f:(fun _ -> init ())

  let sum_exn = List.map2_exn ~f:( +. )

  let hadamard_exn = List.map2_exn ~f:( *. )

  let dot_exn t1 t2 =
    List.fold2_exn t1 t2 ~init:0. ~f:(fun acc x1 x2 -> acc +. (x1 *. x2))

  let map = List.map

  let scale t ~k = List.map t ~f:(( *. ) k)

  let of_list x = x
end

module Matrix = struct
  type t = Vector.t list [@@deriving sexp]

  let create x y ~init = List.init y ~f:(fun _ -> Vector.create x ~init)

  let scale t ~k = List.map t ~f:(Vector.scale ~k)

  let sum_exn t1 t2 = List.map2_exn t1 t2 ~f:Vector.sum_exn

  let product_exn matrix vector =
    List.map matrix ~f:(fun v ->
        (* print_s [%message (vector : Vector.t) (v : Vector.t)]; *)
        Vector.dot_exn vector v)

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

  let v_times_vT v1 v2 = List.map v2 ~f:(fun float -> Vector.scale v1 ~k:float)
end
