namespace SignalProcessing

module FFT =
  module Normalization =
    type T = Asymmetric | Symmetric

  type T

  val create : n : int -> normalization : Normalization.T -> T
  val compute : T -> input : Complex.T array -> output : Complex.T array -> unit
  val computeInverse : T -> input : Complex.T array -> output : Complex.T array -> unit

  val realArrayToCentered : input : float32 array -> centeredOutput : Complex.T array -> unit
  val realArrayFromCentered : output : float32 array -> centeredInput : Complex.T array -> unit
  val sampleArrayToCentered : channel : int -> input : Sample.T array -> centeredOutput : Complex.T array -> unit
  val sampleArrayFromCentered : channel : int -> output : Sample.T array -> centeredInput : Complex.T array -> unit
