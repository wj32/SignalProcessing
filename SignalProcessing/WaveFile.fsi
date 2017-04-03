namespace SignalProcessing

open System

module WaveFile =
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

  val parameters : T -> Parameters.T
  val accessor : T -> SampleAccessor.T
  val get : T -> index : int -> Sample.T
  val set : T -> index : int -> sample : Sample.T -> unit
  val getI : T -> index : int -> SampleI.T
  val setI : T -> index : int -> sample : SampleI.T -> unit

  val openFile : accessType : AccessType.T -> fileName : string -> T
  val createFile : fileName : string -> parameters : Parameters.T -> T
