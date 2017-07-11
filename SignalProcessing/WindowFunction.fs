namespace SignalProcessing

open System

module WindowFunction =
  type T =
    { width : int;
      f : float32 array; }

  let normalizationFactor t h =
    if (t.width - 1) % h <> 0 then
      failwith "(width - 1) must be divisible by the hop size."
    let rec loop acc i =
      if i >= t.width then
        acc
      else
        loop (acc + t.f.[i]) (i + h)
    1.f / loop 0.f 0

  let roundUpPowerOfTwo n =
    let k = n - 1
    let k = k ||| (k >>> 1)
    let k = k ||| (k >>> 2)
    let k = k ||| (k >>> 4)
    let k = k ||| (k >>> 8)
    let k = k ||| (k >>> 16)
    let n' = k + 1
    n'

  let maxSpectrum fftSize wf =
    if fftSize < wf.f.Length then
      invalidArg "fftSize" "fftSize cannot be less than the window width."
    let fft = FFT.create fftSize FFT.Normalization.Asymmetric
    let output = Array.zeroCreate fftSize
    FFT.compute
      fft
      (Array.append wf.f (Array.zeroCreate (fftSize - wf.f.Length)) |> Array.map Complex.createRe)
      output
    Complex.abs output.[0] / float32 fftSize

  let rectangular n =
    { width = n;
      f = Array.init n (fun _ -> 1.f); }

  let triangular n =
    let mid = 0.5f * (float32 (n - 1))
    let scale = 1.f / mid
    { width = n;
      f = Array.init n (fun i -> 1.f - abs (scale * (float32 i - mid))); }

  let hann n =
    let c = 2.f * float32 Math.PI / (float32 (n - 1))
    { width = n;
      f = Array.init n (fun i -> 0.5f * (1.f - cos (c * float32 i))); }

  let hamming n =
    let a = 0.54f
    let b = 1.f - a
    let c = 2.f * float32 Math.PI / (float32 (n - 1))
    { width = n;
      f = Array.init n (fun i -> a - b * cos (c * float32 i)); }

  let blackman n =
    let u = 0.16f
    let a0 = (1.f - u) / 2.f
    let a1 = 0.5f
    let a2 = u / 2.f
    let c1 = 2.f * float32 Math.PI / (float32 (n - 1))
    let c2 = 2.f * c1
    { width = n;
      f = Array.init n (fun i -> a0 - a1 * cos (c1 * float32 i) + a2 * cos (c2 * float32 i)); }
