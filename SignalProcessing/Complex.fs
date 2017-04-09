namespace SignalProcessing

module Complex =
  [<Struct>]
  type T =
    {a : float32; b : float32}

    static member inline (~-)(t) = {a = -t.a; b = -t.b}

    static member inline (~~)(t) = {a = t.a; b = -t.b}

    static member inline (*)(t0 : T, t1 : T) =
      {a = t0.a * t1.a - t0.b * t1.b; b = t0.b * t1.a + t0.a * t1.b}

    static member inline (*)(c : float32, t) = {a = c * t.a; b = c * t.b}

    static member inline (*)(t, c : float32) = {a = t.a * c; b = t.b * c}

    static member (/)(t0 : T, t1 : T) =
      let r2 = t1.a * t1.a + t1.b * t1.b
      {a = (t0.a * t1.a + t0.b * t1.b) / r2; b = (t0.b * t1.a - t0.a * t1.b) / r2}

    static member inline (/)(t, c : float32) = {a = t.a / c; b = t.b / c}

    static member inline (+)(t0, t1) = {a = t0.a + t1.a; b = t0.b + t1.b}

    static member inline (-)(t0, t1) = {a = t0.a - t1.a; b = t0.b - t1.b}

    override t.ToString() = t.a.ToString() + "+" + t.b.ToString() + "i"

  let zero = {a = 0.f; b = 0.f}

  let one = {a = 1.f; b = 0.f}

  let i = {a = 0.f; b = 1.f}

  let inline create a b = {a = float32 a; b = float32 b}

  let inline createRe a = {a = float32 a; b = 0.f}

  let inline createIm b = {a = 0.f; b = float32 b}

  let inline re t = t.a

  let inline im t = t.b

  let inline abs t = sqrt (t.a * t.a + t.b * t.b)

  let inline absSq t = t.a * t.a + t.b * t.b

  let inline arg t = atan2 t.b t.a

  let inline ofPolar (mag : float32) (arg : float32) = {a = mag * cos arg; b = mag * sin arg}

  let inline neg t = {a = -t.a; b = -t.b}

  let inline conj t = {a = t.a; b = -t.b}

  let inline mul t0 t1 = {a = t0.a * t1.a - t0.b * t1.b; b = t0.b * t1.a + t0.a * t1.b}

  let inline mul' t c = {a = t.a * c; b = t.b * c}

  let div t0 t1 =
      let r2 = t1.a * t1.a + t1.b * t1.b
      {a = (t0.a * t1.a + t0.b * t1.b) / r2; b = (t0.b * t1.a - t0.a * t1.b) / r2}

  let inline div' t c = {a = t.a / c; b = t.b / c}

  let inline add t0 t1 = {a = t0.a + t1.a; b = t0.b + t1.b}

  let inline sub t0 t1 = {a = t0.a - t1.a; b = t0.b - t1.b}

  let inline addMul t0 t1 t3 =
    {a = t0.a + t1.a * t3.a - t1.b * t3.b; b = t0.b + t1.b * t3.a + t1.a * t3.b}

  let inline addMul' t0 t1 c = {a = t0.a + t1.a * c; b = t0.b + t1.b * c}

  let inline inv t =
    let r = absSq t
    {a = t.a / r; b = -t.b / r}

  let inline exp t = ofPolar (exp t.a) t.b

  let inline log t = ofPolar (0.5f * log (absSq t)) (arg t)

  let inline log' (c : float32) = ofPolar (0.5f * FSharp.Core.Operators.log (c * c)) (atan2 0.f c)

  let sqrt t =
    let mag = abs t
    if t.b >= 0.f then
      {a = sqrt (0.5f * (mag + t.a)); b = sqrt (0.5f * (mag - t.a))}
    else
      {a = sqrt (0.5f * (mag + t.a)); b = -(sqrt (0.5f * (mag - t.a)))}

  let inline sqrt' (c : float32) =
    if c >= 0.f then
      {a = FSharp.Core.Operators.sqrt c; b = 0.f}
    else
      {a = 0.f; b = FSharp.Core.Operators.sqrt (-c)}
