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
    let modHz = 1.
    for i = 0 to parameters.samples - 1 do
      let sample = W.get input i
      let u = float i / float parameters.sampleRate
      let c = 0.5 * (1. + sin (2. * Math.PI * modHz * u))
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
      let c = float k / float factor
      let sample = (1. - c) * sample1 + c * sample2
      W.set output j sample
      j <- j + 1

  let testAddNoise (input : W.T) (parameters : P.T) (output : W.T) =
    let noiseSinHz = 440.
    let noiseSinAmp = 0.5
    let noiseGaussianScale = 0.2
    let noiseGaussianAmp = 0.2
    let signalAmp = 1. - noiseSinAmp - noiseGaussianAmp
    let random = new Random()
    let boxMuller () =
      let r = sqrt (-2. * log (random.NextDouble()))
      let t = 2. * Math.PI * (random.NextDouble())
      (r * cos t, r * sin t)
    for i = 0 to parameters.samples - 1 do
      let sample = W.get input i
      let u = float i / float parameters.sampleRate
      let noiseSin = sin (2. * Math.PI * noiseSinHz * u)
      let noiseGaussian1, noiseGaussian2 = boxMuller ()
      let sample' =
        signalAmp * sample
        + noiseSinAmp * (Sample.create noiseSin noiseSin)
        + noiseGaussianAmp * noiseGaussianScale * !!(Sample.create noiseGaussian1 noiseGaussian2)
      W.set output i sample'

  let testSkipFrames (input : W.T) (parameters : P.T) (output : W.T) =
    let frameSize = 8001
    let hopSize = 2000
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
        SampleBuffer.add outputBuffer c frame
        SampleBuffer.moveBy outputBuffer hopSize
      SampleBuffer.moveBy inputBuffer hopSize
    SampleBuffer.flush outputBuffer

  [<EntryPoint>]
  let main argv =
      use input = W.openFile AccessType.ReadOnly "D:\\Box\\rag_doll.wav"
      let parameters = W.parameters input
      let output = W.createFile "D:\\Box\\test.wav" parameters

      testSkipFrames input parameters output

      (output :> IDisposable).Dispose()

      printfn "Done."
      stdin.ReadLine() |> ignore
      0
