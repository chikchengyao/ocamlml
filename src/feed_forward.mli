open! Core
open Math

type t

val create :
  int list ->
  init:(unit -> float) ->
  activation:(float -> float) ->
  activation':(float -> float) ->
  t

val run : t -> input:Vector.t -> Vector.t Or_diff.t
