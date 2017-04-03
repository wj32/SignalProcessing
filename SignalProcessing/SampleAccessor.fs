namespace SignalProcessing

module SampleAccessor =
  type Get = int -> Sample.T
  type Set = int -> Sample.T -> unit
  type GetI = int -> SampleI.T
  type SetI = int -> SampleI.T -> unit

  type T =
    { size : int;
      get : Get;
      set : Set;
      getI : GetI;
      setI : SetI; }
