namespace SignalProcessing

module Sample =
  [<Struct>]
  type T =
    {x0 : float32; x1 : float32}

    static member inline (~-)(t) = {x0 = -t.x0; x1 = -t.x1}

    static member inline (*)(c, t) = {x0 = c * t.x0; x1 = c * t.x1}

    static member inline (*)(t, c) = {x0 = t.x0 * c; x1 = t.x1 * c}

    static member inline (/)(t, c) = {x0 = t.x0 / c; x1 = t.x1 / c}

    static member inline (+)(t0, t1) = {x0 = t0.x0 + t1.x0; x1 = t0.x1 + t1.x1}

    static member inline (-)(t0, t1) = {x0 = t0.x0 - t1.x0; x1 = t0.x1 - t1.x1}

    static member inline (!!)(t) =
      { x0 = if t.x0 > 1.f then 1.f else if t.x0 < -1.f then -1.f else t.x0;
        x1 = if t.x1 > 1.f then 1.f else if t.x1 < -1.f then -1.f else t.x1; }

    override t.ToString() = "(" + t.x0.ToString() + ", " + t.x1.ToString() + ")"

  let zero = {x0 = 0.f; x1 = 0.f}

  let inline create x0 x1 = {x0 = float32 x0; x1 = float32 x1}

  let inline neg t = {x0 = -t.x0; x1 = -t.x1}

  let inline mul t c = {x0 = t.x0 * c; x1 = t.x1 * c}

  let inline div t c = {x0 = t.x0 / c; x1 = t.x1 / c}

  let inline add t0 t1 = {x0 = t0.x0 + t1.x0; x1 = t0.x1 + t1.x1}

  let inline sub t0 t1 = {x0 = t0.x0 - t1.x0; x1 = t0.x1 - t1.x1}

  let inline addMul t0 t1 c = {x0 = t0.x0 + t1.x0 * c; x1 = t0.x1 + t1.x1 * c}

  let inline clamp (t : T) = !!t

module SampleC =
  [<Struct>]
  type T =
    {x0 : Complex.T; x1 : Complex.T}

    static member inline (~-)(t) = {x0 = Complex.neg t.x0; x1 = Complex.neg t.x1}

    static member inline (~~)(t) = {x0 = Complex.conj t.x0; x1 = Complex.conj t.x1}

    static member inline (*)(c, t) = {x0 = Complex.mul' t.x0 c; x1 = Complex.mul' t.x1 c}

    static member inline (*)(t, c) = {x0 = Complex.mul' t.x0 c; x1 = Complex.mul' t.x1 c}

    static member inline (/)(t, c) = {x0 = Complex.div' t.x0 c; x1 = Complex.div' t.x1 c}

    static member inline (+)(t0, t1) = {x0 = Complex.add t0.x0 t1.x0; x1 = Complex.add t0.x1 t1.x1}

    static member inline (-)(t0, t1) = {x0 = Complex.sub t0.x0 t1.x0; x1 = Complex.sub t0.x1 t1.x1}

    override t.ToString() = "(" + t.x0.ToString() + ", " + t.x1.ToString() + ")"

  let zero = {x0 = Complex.zero; x1 = Complex.zero}

  let inline create x0 x1 = {x0 = x0; x1 = x1}

  let inline createRe (x0 : float32) (x1 : float32) =
    {x0 = Complex.createRe x0; x1 = Complex.createRe x1}

  let inline createIm (x0 : float32) (x1 : float32) =
    {x0 = Complex.createIm x0; x1 = Complex.createIm x1}

  let inline re t = {Sample.x0 = Complex.re t.x0; Sample.x1 = Complex.re t.x1}

  let inline im t = {Sample.x0 = Complex.im t.x0; Sample.x1 = Complex.im t.x1}

  let inline abs t = {Sample.x0 = Complex.abs t.x0; Sample.x1 = Complex.abs t.x1}

  let inline arg t = {Sample.x0 = Complex.arg t.x0; Sample.x1 = Complex.arg t.x1}

  let inline neg t = {x0 = Complex.neg t.x0; x1 = Complex.neg t.x1}

  let inline conj t = {x0 = Complex.conj t.x0; x1 = Complex.conj t.x1}

  let inline mul t c = {x0 = Complex.mul' t.x0 c; x1 = Complex.mul' t.x1 c}

  let inline div t c = {x0 = Complex.div' t.x0 c; x1 = Complex.div' t.x1 c}

  let inline add t0 t1 = {x0 = Complex.add t0.x0 t1.x0; x1 = Complex.add t0.x1 t1.x1}

  let inline sub t0 t1 = {x0 = Complex.sub t0.x0 t1.x0; x1 = Complex.sub t0.x1 t1.x1}

module SampleI =
  [<Struct>]
  type T =
    {x0 : int16; x1 : int16}

    static member inline (~-)(t) = {x0 = -t.x0; x1 = -t.x1}

    static member inline (*)(c, t) = {x0 = c * t.x0; x1 = c * t.x1}

    static member inline (*)(t, c) = {x0 = t.x0 * c; x1 = t.x1 * c}

    static member inline (/)(t, c) = {x0 = t.x0 / c; x1 = t.x1 / c}

    static member inline (+)(t0, t1) = {x0 = t0.x0 + t1.x0; x1 = t0.x1 + t1.x1}

    static member inline (-)(t0, t1) = {x0 = t0.x0 - t1.x0; x1 = t0.x1 - t1.x1}

    override t.ToString() = "(" + t.x0.ToString() + ", " + t.x1.ToString() + ")"

  let zero = {x0 = 0s; x1 = 0s}

  let inline create x0 x1 = {x0 = int16 x0; x1 = int16 x1}

  let inline neg t = {x0 = -t.x0; x1 = -t.x1}

  let inline mul t c = {x0 = t.x0 * c; x1 = t.x1 * c}

  let inline div t c = {x0 = t.x0 / c; x1 = t.x1 / c}

  let inline add t0 t1 = {x0 = t0.x0 + t1.x0; x1 = t0.x1 + t1.x1}

  let inline sub t0 t1 = {x0 = t0.x0 - t1.x0; x1 = t0.x1 - t1.x1}
