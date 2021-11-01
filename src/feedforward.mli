open! Core
open Math

type t

val create :
  int list ->
  init:(unit -> float) ->
  activation:(Vector.t -> Vector.t) ->
  activation':(Vector.t -> Vector.t) ->
  t

val run_exn : t -> input:Vector.t -> Vector.t

val run_and_backpropagate_exn :
  t ->
  input:Vector.t ->
  cost':(output:Vector.t -> Vector.t) ->
  step:float ->
  t * Vector.t

val train_exn :
  t ->
  inputs:Vector.t list ->
  outputs:Vector.t list ->
  cost':(expected:Vector.t -> output:Vector.t -> Vector.t) ->
  (* n starts at 1 *)
  step:(i:int -> float) ->
  t
