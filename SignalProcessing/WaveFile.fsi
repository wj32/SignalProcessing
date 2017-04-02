namespace SignalProcessing

open System

module WaveFile =
  module Access =
    type T = ReadOnly | ReadWrite

  module Parameters =
    type T =
      { channels : int; (* sample/msample *)
        bitDepth : int; (* bit/sample *)
        sampleRate : int; (* msample/second *)
        samples : int; (* msample *) }

  exception InvalidFormatException of message : string

  [<Sealed>]
  type T =
    interface IDisposable

  val openFile : access : Access.T -> fileName : string -> T
  val createFile : fileName : string -> parameters : Parameters.T -> T

  val parameters : T -> Parameters.T

  val get : T -> index : int -> Sample.T
  val set : T -> index : int -> sample : Sample.T -> unit
  val getF : T -> index : int -> SampleF.T
  val setF : T -> index : int -> sample : SampleF.T -> unit
