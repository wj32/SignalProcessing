namespace SignalProcessing

open System

module Main =
  module W = WaveFile
  module P = W.Parameters

  let testDiff (input : W.T) (parameters : P.T) (output : W.T) =
    for i = 0 to parameters.samples - 1 do
      let sample = W.getI input i
      let diff = sample.x0 / 2s - sample.x1 / 2s
      let sample' = SampleI.create diff diff
      W.setI output i sample'

  let testModAmp (input : W.T) (parameters : P.T) (output : W.T) =
    let modHz = 1.f
    for i = 0 to parameters.samples - 1 do
      let sample = W.get input i
      let u = float32 i / float32 parameters.sampleRate
      let c = 0.5f * (1.f + sin (2.f * float32 Math.PI * modHz * u))
      let sample' = c * sample
      W.set output i sample'

  let testSkip (input : W.T) (parameters : P.T) (output : W.T) =
    let factor = 3
    let mutable j = 0
    for i = 0 to parameters.samples - 1 do
      if i % factor = 0 then
        let sample = W.get input i
        W.set output j sample
        j <- j + 1

  let testExpand (input : W.T) (parameters : P.T) (output : W.T) =
    let factor = 2
    let mutable j = 0
    let mutable sample1 = Sample.zero
    let mutable sample2 = Sample.zero
    for i = 0 to parameters.samples - 1 do
      let k = i % factor
      if k = 0 then
        sample1 <- sample2
        sample2 <- W.get input (i / factor)
      let c = float32 k / float32 factor
      let sample = (1.f - c) * sample1 + c * sample2
      W.set output j sample
      j <- j + 1

  let testAddNoise (input : W.T) (parameters : P.T) (output : W.T) =
    let noiseSinHz = 440.f
    let noiseSinAmp = 0.5f
    let noiseGaussianScale = 0.2f
    let noiseGaussianAmp = 0.2f
    let signalAmp = 1.f - noiseSinAmp - noiseGaussianAmp
    let random = new Random()
    let boxMuller () =
      let r = sqrt (-2. * log (random.NextDouble()))
      let t = 2. * Math.PI * (random.NextDouble())
      (r * cos t, r * sin t)
    for i = 0 to parameters.samples - 1 do
      let sample = W.get input i
      let u = float32 i / float32 parameters.sampleRate
      let noiseSin = sin (2.f * float32 Math.PI * noiseSinHz * u)
      let noiseGaussian1, noiseGaussian2 = boxMuller ()
      let sample' =
        signalAmp * sample
        + noiseSinAmp * (Sample.create noiseSin noiseSin)
        + noiseGaussianAmp * noiseGaussianScale * !!(Sample.create noiseGaussian1 noiseGaussian2)
      W.set output i sample'

  let testSkipFrames (input : W.T) (parameters : P.T) (output : W.T) =
    let frameSize = 12001
    let hopSize = 2000
    let frames = (parameters.samples + hopSize - 1) / hopSize
    let factor = 2
    let inputBuffer = SampleBuffer.create (W.accessor input) AccessType.ReadOnly 50000
    let outputBuffer = SampleBuffer.create (W.accessor output) AccessType.WriteOnly 50000
    let frame = Array.zeroCreate frameSize
    let wf = WindowFunction.blackman frameSize
    let c = WindowFunction.normalizationFactor wf hopSize
    for i = 0 to frames - 1 do
      if i % factor = 0 then
        SampleBuffer.window inputBuffer wf frame
        SampleBuffer.add outputBuffer c frame
        SampleBuffer.moveBy outputBuffer hopSize
      SampleBuffer.moveBy inputBuffer hopSize
    SampleBuffer.flush outputBuffer

  let testExpandFrames (input : W.T) (parameters : P.T) (output : W.T) =
    let frameSize = 2001
    let hopSize = 500
    let frames = (parameters.samples + hopSize - 1) / hopSize
    let factor = 2
    let inputBuffer = SampleBuffer.create (W.accessor input) AccessType.ReadOnly 50000
    let outputBuffer = SampleBuffer.create (W.accessor output) AccessType.WriteOnly 50000
    let frame = Array.zeroCreate frameSize
    let wf = WindowFunction.hamming frameSize
    let c = WindowFunction.normalizationFactor wf hopSize
    for i = 0 to frames - 1 do
      if i % factor = 0 then
        SampleBuffer.window inputBuffer wf frame
        SampleBuffer.moveBy inputBuffer hopSize
      SampleBuffer.add outputBuffer c frame
      SampleBuffer.moveBy outputBuffer hopSize
    SampleBuffer.flush outputBuffer

  let modTwoPi (x : float32) = float32 (Math.IEEERemainder(float x, 2. * Math.PI))

  let testAnalyzeFrames (input : W.T) (parameters : P.T) (output : W.T) =
    let frameSize = 8001
    let hopSize = 2000
    let fftSize = 16384 * 2
    let binSize = float32 parameters.sampleRate / float32 fftSize
    let hopTime = float32 hopSize / float32 parameters.sampleRate
    let frames = (parameters.samples + hopSize - 1) / hopSize
    let fft = FFT.create fftSize FFT.Normalization.Symmetric
    let inputBuffer = SampleBuffer.create (W.accessor input) AccessType.ReadOnly 50000
    let outputBuffer = SampleBuffer.create (W.accessor output) AccessType.WriteOnly 50000
    let frame = Array.zeroCreate frameSize
    let fftInput = Array.zeroCreate fftSize
    let fftOutput = Array.zeroCreate fftSize
    let wf = WindowFunction.hann frameSize
    let c = WindowFunction.normalizationFactor wf hopSize

    // Phase vocoder
    let prevPhase = Array.init 2 (fun _ -> Array.zeroCreate (fftSize / 2 + 1))
    let sumPhase = Array.init 2 (fun _ -> Array.zeroCreate (fftSize / 2 + 1))
    let binMag = Array.zeroCreate (fftSize / 2 + 1)
    let binFreq = Array.zeroCreate (fftSize / 2 + 1)
    let binMag' = Array.zeroCreate (fftSize / 2 + 1)
    let binFreq' = Array.zeroCreate (fftSize / 2 + 1)
    let twoPi = 2.f * float32 Math.PI
    let phaseHop = twoPi * binSize * hopTime
    let phaseDiffExpected =
      Array.init (fftSize / 2 + 1) (fun i -> modTwoPi (float32 i * phaseHop))

    let passCutoffHz = 440.f
    let passCutoffBin = passCutoffHz / binSize
    let rampWidthHz = 320.f
    let rampWidthBins = rampWidthHz / binSize
    let rampLeftBin = passCutoffBin - rampWidthBins / 2.f
    let rampRightBin = passCutoffBin + rampWidthBins / 2.f

    let lowPass i =
      let i = float32 i
      if i < rampLeftBin then
        1.f
      else if i > rampRightBin then
        0.f
      else
        (rampRightBin - i) / rampWidthBins

    let highPass i = 1.f - lowPass i

    let mulFilter =
      let f = lowPass
      let a = Array.zeroCreate fftSize
      // Enforce symmetry
      for i = 0 to fftSize / 2 do
        a.[i] <- f i
      for i = fftSize / 2 + 1 to fftSize - 1 do
        a.[i] <- a.[fftSize - i]
      a

    let mutable norm = 0.f

    for frameIndex = 0 to frames - 1 do
      SampleBuffer.window inputBuffer wf frame
      for channel = 0 to 1 do
        FFT.sampleArrayToCentered channel frame fftInput
        FFT.compute fft fftInput fftOutput

        //for j = 0 to fftSize - 1 do
        //  fftOutput.[j] <- Complex.mul' fftOutput.[j] mulFilter.[j]

        // Phase vocoder
        for j = 0 to fftSize / 2 do
          let phase = Complex.arg fftOutput.[j]
          let phaseDiff = phase - prevPhase.[channel].[j]
          prevPhase.[channel].[j] <- phase
          let phaseDiffDev = phaseDiff - phaseDiffExpected.[j]
          let phaseDiffDev = modTwoPi phaseDiffDev
          let phaseDiffDevFrac = phaseDiffDev / phaseHop
          binFreq.[j] <- (float32 j + phaseDiffDevFrac) * binSize
          binMag.[j] <- Complex.abs fftOutput.[j]

        // Shift
        let halfTone = 2.f ** (1.f / 12.f)
        let shiftFactor = halfTone ** (6.f)
        Array.fill binMag' 0 (fftSize / 2 + 1) 0.f
        Array.fill binFreq' 0 (fftSize / 2 + 1) 0.f
        for j = 0 to fftSize / 2 do
          let j' = int (float32 j * shiftFactor)
          if j' <= fftSize / 2 then
            binMag'.[j'] <- binMag'.[j'] + binMag.[j]
            binFreq'.[j'] <- binFreq.[j] * shiftFactor
        //for j = 0 to fftSize / 2 do
        //  let j' = int (float32 j / shiftFactor)
        //  if j' <= fftSize / 2 then
        //    binMag'.[j] <- binMag.[j']
        //    binFreq'.[j] <- binFreq.[j'] * shiftFactor

        // Phase vocoder
        for j = 0 to fftSize / 2 do
          let freqDevFrac = binFreq'.[j] / binSize - float32 j
          let phaseDiffDev = freqDevFrac * phaseHop
          let phaseDiff = phaseDiffExpected.[j] + phaseDiffDev
          let phase = sumPhase.[channel].[j] + phaseDiff
          sumPhase.[channel].[j] <- phase
          fftOutput.[j] <- Complex.ofPolar binMag'.[j] phase

        for j = fftSize / 2 + 1 to fftSize - 1 do
          fftOutput.[j] <- Complex.conj fftOutput.[fftSize - j]

        FFT.computeInverse fft fftOutput fftInput
        FFT.sampleArrayFromCentered channel frame fftInput
      SampleBuffer.add' outputBuffer c wf frame
      norm <- max norm (SampleBuffer.norm outputBuffer frameSize)
      SampleBuffer.moveBy outputBuffer hopSize
      SampleBuffer.moveBy inputBuffer hopSize
    SampleBuffer.flush outputBuffer
    printfn "Norm: %f" norm

  [<EntryPoint>]
  let main argv =
      use input = W.openFile AccessType.ReadOnly "D:\\Box\\rag_doll.wav"
      let parameters = W.parameters input
      let output = W.createFile "D:\\Box\\test.wav" parameters

      testAnalyzeFrames input parameters output

      (output :> IDisposable).Dispose()

      printfn "Done."
      stdin.ReadLine() |> ignore
      0
