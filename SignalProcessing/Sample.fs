﻿namespace SignalProcessing

module Sample =
  [<Struct>]
  type T =
    {x1 : int16; x2 : int16}

    static member inline (~-)(t) = {x1 = -t.x1; x2 = -t.x2}
    static member inline (*)(t, c) = {x1 = t.x1 * c; x2 = t.x2 * c}
    static member inline (/)(t, c) = {x1 = t.x1 / c; x2 = t.x2 / c}
    static member inline (+)(t1, t2) = {x1 = t1.x1 + t2.x1; x2 = t1.x2 + t2.x2}
    static member inline (-)(t1, t2) = {x1 = t1.x1 - t2.x1; x2 = t1.x2 - t2.x2}

    override t.ToString() = "(" + t.x1.ToString() + ", " + t.x2.ToString() + ")"

  let zero = {x1 = 0s; x2 = 0s}

  let inline create x1 x2 = {x1 = x1; x2 = x2}

  let inline neg (t : T) = -t
  let inline mul (t : T) c = t * c
  let inline div (t : T) c = t / c
  let inline add (t1 : T) (t2 : T) = t1 + t2
  let inline sub (t1 : T) (t2 : T) = t1 - t2

module SampleF =
  [<Struct>]
  type T =
    {x1 : float32; x2 : float32}

    static member inline (~-)(t) = {x1 = -t.x1; x2 = -t.x2}
    static member inline (*)(t, c : float32) = {x1 = t.x1 * c; x2 = t.x2 * c}
    static member inline (*)(t, c : float) = {x1 = t.x1 * float32 c; x2 = t.x2 * float32 c}
    static member inline (/)(t, c : float32) = {x1 = t.x1 / c; x2 = t.x2 / c}
    static member inline (/)(t, c : float) = {x1 = t.x1 / float32 c; x2 = t.x2 / float32 c}
    static member inline (+)(t1, t2) = {x1 = t1.x1 + t2.x1; x2 = t1.x2 + t2.x2}
    static member inline (-)(t1, t2) = {x1 = t1.x1 - t2.x1; x2 = t1.x2 - t2.x2}

    static member inline (!!)(t) =
      { x1 = if t.x1 > 1.f then 1.f else if t.x1 < -1.f then -1.f else t.x1;
        x2 = if t.x2 > 1.f then 1.f else if t.x2 < -1.f then -1.f else t.x2; }

    override t.ToString() = "(" + t.x1.ToString() + ", " + t.x2.ToString() + ")"

  let zero = {x1 = 0.f; x2 = 0.f}

  let inline create x1 x2 = {x1 = x1; x2 = x2}

  let inline neg (t : T) = -t
  let inline mul (t : T) c = t * c
  let inline div (t : T) c = t / c
  let inline add (t1 : T) (t2 : T) = t1 + t2
  let inline sub (t1 : T) (t2 : T) = t1 - t2

  let inline clamp (t : T) = !!t