open! Core
open Math

type t

val create :
  int list ->
  init:(unit -> float) ->
  activation:(float -> float) ->
  activation':(float -> float) ->
  t

val run_exn : t -> input:Vector.t -> Vector.t

val run_and_backpropagate_exn :
  t ->
  input:Vector.t ->
  cost':(output:Vector.t -> Vector.t) ->
  step:float ->
  t * Vector.t
