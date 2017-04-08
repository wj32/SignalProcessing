namespace SignalProcessing

open System

module Main =
  module W = WaveFile
  module P = W.Parameters

  let testDiff (input : W.T) (parameters : P.T) (output : W.T) =
    for i = 0 to parameters.samples - 1 do
      let sample = W.getI input i
      let diff = sample.x1 / 2s - sample.x2 / 2s
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

  let testAnalyzeFrames (input : W.T) (parameters : P.T) (output : W.T) =
    let frameSize = 8001
    let hopSize = 2000
    let fftSize = 16384 * 2
    let binSize = float32 parameters.sampleRate / float32 fftSize
    let frames = (parameters.samples + hopSize - 1) / hopSize
    let fft = FFT.create fftSize FFT.Normalization.Symmetric
    let inputBuffer = SampleBuffer.create (W.accessor input) AccessType.ReadOnly 50000
    let outputBuffer = SampleBuffer.create (W.accessor output) AccessType.WriteOnly 50000
    let frame = Array.zeroCreate frameSize
    let fftInput = Array.zeroCreate fftSize
    let fftOutput = Array.zeroCreate fftSize
    let wf = WindowFunction.blackman frameSize
    let c = 0.5f * WindowFunction.normalizationFactor wf hopSize

    let passCutoffHz = 440.f
    let passCutoffBin = passCutoffHz / binSize
    let rampWidthHz = 40.f
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

    let filterToApply =
      let f = lowPass
      let a = Array.zeroCreate fftSize
      // Enforce symmetry
      for i = 0 to fftSize / 2 do
        a.[i] <- f i
      for i = fftSize / 2 + 1 to fftSize - 1 do
        a.[i] <- a.[fftSize - i]
      a

    for i = 0 to frames - 1 do
      SampleBuffer.window inputBuffer wf frame
      for channel = 1 to 2 do
        FFT.sampleArrayToCentered channel frame fftInput
        FFT.compute fft fftInput fftOutput

        for j = 0 to fftSize - 1 do
          fftOutput.[j] <- Complex.mul' fftOutput.[j] filterToApply.[j]

        FFT.computeInverse fft fftOutput fftInput
        FFT.sampleArrayFromCentered channel frame fftInput
      SampleBuffer.add outputBuffer c frame
      SampleBuffer.moveBy outputBuffer hopSize
      SampleBuffer.moveBy inputBuffer hopSize
    SampleBuffer.flush outputBuffer

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
