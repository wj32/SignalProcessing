namespace SignalProcessing

module Sample =
  [<Struct>]
  type T =
    {x1 : float32; x2 : float32}

    static member inline (~-)(t) = {x1 = -t.x1; x2 = -t.x2}

    static member inline (*)(c, t) = {x1 = c * t.x1; x2 = c * t.x2}

    static member inline (*)(t, c) = {x1 = t.x1 * c; x2 = t.x2 * c}

    static member inline (/)(t, c) = {x1 = t.x1 / c; x2 = t.x2 / c}

    static member inline (+)(t1, t2) = {x1 = t1.x1 + t2.x1; x2 = t1.x2 + t2.x2}

    static member inline (-)(t1, t2) = {x1 = t1.x1 - t2.x1; x2 = t1.x2 - t2.x2}

    static member inline (!!)(t) =
      { x1 = if t.x1 > 1.f then 1.f else if t.x1 < -1.f then -1.f else t.x1;
        x2 = if t.x2 > 1.f then 1.f else if t.x2 < -1.f then -1.f else t.x2; }

    override t.ToString() = "(" + t.x1.ToString() + ", " + t.x2.ToString() + ")"

  let zero = {x1 = 0.f; x2 = 0.f}

  let inline create x1 x2 = {x1 = float32 x1; x2 = float32 x2}

  let inline neg t = {x1 = -t.x1; x2 = -t.x2}

  let inline mul t c = {x1 = t.x1 * c; x2 = t.x2 * c}

  let inline div t c = {x1 = t.x1 / c; x2 = t.x2 / c}

  let inline add t1 t2 = {x1 = t1.x1 + t2.x1; x2 = t1.x2 + t2.x2}

  let inline sub t1 t2 = {x1 = t1.x1 - t2.x1; x2 = t1.x2 - t2.x2}

  let inline addMul t1 t2 c = {x1 = t1.x1 + t2.x1 * c; x2 = t1.x2 + t2.x2 * c}

  let inline clamp (t : T) = !!t

module SampleC =
  [<Struct>]
  type T =
    {x1 : Complex.T; x2 : Complex.T}

    static member inline (~-)(t) = {x1 = Complex.neg t.x1; x2 = Complex.neg t.x2}

    static member inline (~~)(t) = {x1 = Complex.conj t.x1; x2 = Complex.conj t.x2}

    static member inline (*)(c, t) = {x1 = Complex.mul' t.x1 c; x2 = Complex.mul' t.x2 c}

    static member inline (*)(t, c) = {x1 = Complex.mul' t.x1 c; x2 = Complex.mul' t.x2 c}

    static member inline (/)(t, c) = {x1 = Complex.div' t.x1 c; x2 = Complex.div' t.x2 c}

    static member inline (+)(t1, t2) = {x1 = Complex.add t1.x1 t2.x1; x2 = Complex.add t1.x2 t2.x2}

    static member inline (-)(t1, t2) = {x1 = Complex.sub t1.x1 t2.x1; x2 = Complex.sub t1.x2 t2.x2}

    override t.ToString() = "(" + t.x1.ToString() + ", " + t.x2.ToString() + ")"

  let zero = {x1 = Complex.zero; x2 = Complex.zero}

  let inline create x1 x2 = {x1 = x1; x2 = x2}

  let inline createRe (x1 : float32) (x2 : float32) =
    {x1 = Complex.createRe x1; x2 = Complex.createRe x2}

  let inline createIm (x1 : float32) (x2 : float32) =
    {x1 = Complex.createIm x1; x2 = Complex.createIm x2}

  let inline re t = {Sample.x1 = Complex.re t.x1; Sample.x2 = Complex.re t.x2}

  let inline im t = {Sample.x1 = Complex.im t.x1; Sample.x2 = Complex.im t.x2}

  let inline abs t = {Sample.x1 = Complex.abs t.x1; Sample.x2 = Complex.abs t.x2}

  let inline arg t = {Sample.x1 = Complex.arg t.x1; Sample.x2 = Complex.arg t.x2}

  let inline neg t = {x1 = Complex.neg t.x1; x2 = Complex.neg t.x2}

  let inline conj t = {x1 = Complex.conj t.x1; x2 = Complex.conj t.x2}

  let inline mul t c = {x1 = Complex.mul' t.x1 c; x2 = Complex.mul' t.x2 c}

  let inline div t c = {x1 = Complex.div' t.x1 c; x2 = Complex.div' t.x2 c}

  let inline add t1 t2 = {x1 = Complex.add t1.x1 t2.x1; x2 = Complex.add t1.x2 t2.x2}

  let inline sub t1 t2 = {x1 = Complex.sub t1.x1 t2.x1; x2 = Complex.sub t1.x2 t2.x2}

module SampleI =
  [<Struct>]
  type T =
    {x1 : int16; x2 : int16}

    static member inline (~-)(t) = {x1 = -t.x1; x2 = -t.x2}

    static member inline (*)(c, t) = {x1 = c * t.x1; x2 = c * t.x2}

    static member inline (*)(t, c) = {x1 = t.x1 * c; x2 = t.x2 * c}

    static member inline (/)(t, c) = {x1 = t.x1 / c; x2 = t.x2 / c}

    static member inline (+)(t1, t2) = {x1 = t1.x1 + t2.x1; x2 = t1.x2 + t2.x2}

    static member inline (-)(t1, t2) = {x1 = t1.x1 - t2.x1; x2 = t1.x2 - t2.x2}

    override t.ToString() = "(" + t.x1.ToString() + ", " + t.x2.ToString() + ")"

  let zero = {x1 = 0s; x2 = 0s}

  let inline create x1 x2 = {x1 = int16 x1; x2 = int16 x2}

  let inline neg t = {x1 = -t.x1; x2 = -t.x2}

  let inline mul t c = {x1 = t.x1 * c; x2 = t.x2 * c}

  let inline div t c = {x1 = t.x1 / c; x2 = t.x2 / c}

  let inline add t1 t2 = {x1 = t1.x1 + t2.x1; x2 = t1.x2 + t2.x2}

  let inline sub t1 t2 = {x1 = t1.x1 - t2.x1; x2 = t1.x2 - t2.x2}
