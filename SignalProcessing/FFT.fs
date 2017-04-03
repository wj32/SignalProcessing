namespace SignalProcessing

open System

module FFT =
  module Normalization =
    type T = Asymmetric | Symmetric

  type T =
    { n : int;
      exponent : int;
      factors : Complex.T array array;
      normalizationFactor : float32;
      normalizationInverseFactor : float32; }

  let log2 n =
    let rec loop k =
      if k > 30 then
        None
      else
        let n' = 1 <<< k
        if n = n' then
          Some k
        else
          loop (k + 1)
    loop 0

  let create n normalization =
    let exponent =
      match log2 n with
      | Some exponent -> exponent
      | None -> invalidArg "n" "n must be a power of two."
    let factors = Array.zeroCreate (exponent + 1)
    for k = 1 to exponent do
      let m = 1 <<< k
      let root = Complex.fromPolar 1.f (-2.f * float32 Math.PI / float32 m)
      let cs = Array.zeroCreate (m / 2)
      cs.[0] <- Complex.one
      for j = 1 to m / 2 - 1 do
        cs.[j] <- Complex.mul cs.[j - 1] root
      factors.[k] <- cs
    { n = n;
      exponent = exponent;
      factors = factors;
      normalizationFactor =
        match normalization with
        | Normalization.Asymmetric -> 1.f
        | Normalization.Symmetric -> 1.f / sqrt (float32 n);
      normalizationInverseFactor =
        match normalization with
        | Normalization.Asymmetric -> 1.f / float32 n
        | Normalization.Symmetric -> 1.f / sqrt (float32 n); }

  let inline computeInternal t factor (input : Complex.T array) (output : Complex.T array) =
    let rec fft n k inputStart inputStride outputStart =
      if k = 0 then
        output.[outputStart] <- factor * input.[inputStart]
      else
        fft (n / 2) (k - 1) inputStart (inputStride * 2) outputStart
        fft (n / 2) (k - 1) (inputStart + inputStride) (inputStride * 2) (outputStart + n / 2)
        for j = 0 to n / 2 - 1 do
          let z = output.[outputStart + j]
          let w = Complex.mul output.[outputStart + n / 2 + j] t.factors.[k].[j]
          output.[outputStart + j] <- Complex.add z w
          output.[outputStart + n / 2 + j] <- Complex.sub z w
    fft t.n t.exponent 0 1 0

  let compute t input output = computeInternal t t.normalizationFactor input output

  let computeInverse t input output =
    computeInternal t t.normalizationInverseFactor input output
    Array.Reverse(output)

  let realArrayToCentered (a : float32 array) (c : Complex.T array) =
    let startSize = a.Length / 2
    for i = 0 to startSize - 1 do
      c.[c.Length - startSize + i] <- Complex.createRe a.[i]
    for i = 0 to a.Length - startSize - 1 do
      c.[i] <- Complex.createRe a.[startSize + i]
    for i = a.Length - startSize to c.Length - startSize - 1 do
      c.[i] <- Complex.zero

  let realArrayFromCentered (a : float32 array) (c : Complex.T array) =
    let startSize = a.Length / 2
    for i = 0 to startSize - 1 do
      a.[i] <- Complex.re c.[c.Length - startSize + i]
    for i = 0 to a.Length - startSize - 1 do
      a.[startSize + i] <- Complex.re c.[i]

  let sampleArrayToCentered channel (a : Sample.T array) (c : Complex.T array) =
    let startSize = a.Length / 2
    match channel with
    | 1 ->
      for i = 0 to startSize - 1 do
        c.[c.Length - startSize + i] <- Complex.createRe a.[i].x1
      for i = 0 to a.Length - startSize - 1 do
        c.[i] <- Complex.createRe a.[startSize + i].x1
    | 2 ->
      for i = 0 to startSize - 1 do
        c.[c.Length - startSize + i] <- Complex.createRe a.[i].x2
      for i = 0 to a.Length - startSize - 1 do
        c.[i] <- Complex.createRe a.[startSize + i].x2
    | _ -> failwith "Invalid channel number"
    for i = a.Length - startSize to c.Length - startSize - 1 do
      c.[i] <- Complex.zero

  let sampleArrayFromCentered channel (a : Sample.T array) (c : Complex.T array) =
    let startSize = a.Length / 2
    match channel with
    | 1 ->
      for i = 0 to startSize - 1 do
        a.[i] <- {Sample.x1 = Complex.re c.[c.Length - startSize + i]; Sample.x2 = a.[i].x2}
      for i = 0 to a.Length - startSize - 1 do
        a.[startSize + i] <- {Sample.x1 = Complex.re c.[i]; Sample.x2 = a.[startSize + i].x2}
    | 2 ->
      for i = 0 to startSize - 1 do
        a.[i] <- {Sample.x1 = a.[i].x1; Sample.x2 = Complex.re c.[c.Length - startSize + i]}
      for i = 0 to a.Length - startSize - 1 do
        a.[startSize + i] <- {Sample.x1 = a.[startSize + i].x1; Sample.x2 = Complex.re c.[i]}
    | _ -> failwith "Invalid channel number"
