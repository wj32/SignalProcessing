namespace SignalProcessing

open FSharp.Charting
open FSharp.Charting.ChartTypes
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

  let modTwoPi (x : float32) = float32 (Math.IEEERemainder(float x, 2. * Math.PI))

  let testSinusoidalModel (input : W.T) (parameters : P.T) (output : W.T) =
    let frameSize = 6000 / 2 + 1
    let hopSize = 1500 / 2
    let fftSize = 16384 * 2
    let binSize = float32 parameters.sampleRate / float32 fftSize
    let hopTime = float32 hopSize / float32 parameters.sampleRate
    let frames = (parameters.samples + hopSize - 1) / hopSize
    let fft = FFT.create fftSize FFT.Normalization.Asymmetric
    let inputBuffer = SampleBuffer.create (W.accessor input) AccessType.ReadOnly 50000
    let outputBuffer = SampleBuffer.create (W.accessor output) AccessType.WriteOnly 50000
    let frame = Array.zeroCreate frameSize
    let fftInput = Array.zeroCreate fftSize
    let fftOutput = Array.zeroCreate fftSize
    let wf = WindowFunction.hann frameSize
    let invMaxSpectrumOfWf = 1.f / WindowFunction.maxSpectrum fftSize wf
    let c = WindowFunction.normalizationFactor wf hopSize

    let mutable norm = 0.f

    let peakCount = 100
    let heap = Heap.createWithCapacity peakCount

    for frameIndex = 0 to frames - 1 do
      SampleBuffer.window inputBuffer wf frame
      for channel = 0 to 1 do
        FFT.sampleArrayToCentered channel frame fftInput
        FFT.compute fft fftInput fftOutput

        // Reduce to top peaks (peakCount)
        let top () =
          Heap.clear heap

          let mutable c0 = 0.f
          let mutable c1 = 0.f
          for j = 0 to fftSize / 2 do
            let c = Complex.abs fftOutput.[j]
            if (c0 < c1) && (c1 > c) then
              if Heap.count heap < peakCount then
                Heap.add heap (-c1, j - 1, fftOutput.[j])
              else
                let (c', _, _) = Heap.top heap
                if c1 > -c' then
                  Heap.replace heap (-c1, j - 1, fftOutput.[j]) |> ignore
            c0 <- c1
            c1 <- c
          Array.fill fftOutput 0 fftOutput.Length Complex.zero
          for (_, index, original) in Heap.toArray heap do
            fftOutput.[index] <- Complex.mul' original invMaxSpectrumOfWf

        top ()

        for j = fftSize / 2 + 1 to fftSize - 1 do
          fftOutput.[j] <- Complex.conj fftOutput.[fftSize - j]

        FFT.computeInverse fft fftOutput fftInput
        FFT.sampleArrayFromCentered channel frame fftInput
      SampleBuffer.add' outputBuffer c wf frame
      norm <- max norm (SampleBuffer.norm outputBuffer frameSize)
      SampleBuffer.moveBy outputBuffer hopSize
      SampleBuffer.moveBy inputBuffer hopSize
      if frameIndex % 100 = 0 then
        printf
          "\rProcessed frame %d / %d (%.1f%%)"
          (frameIndex + 1)
          frames
          (float (frameIndex + 1) / float frames * 100.)
    SampleBuffer.flush outputBuffer
    printfn ""
    printfn "Norm: %f" norm

  let showChart (chart : GenericChart) =
    let f = chart.ShowChart()
    f.TopMost <- false
    System.Windows.Forms.Application.Run f

  module HarmonicModel =
    type Harmonic =
      { freq : float32;
        mutable partials : Partial list;
        mutable originalAmplitude : float32;
        mutable fitAmplitude : float32; }
    and Sinusoid =
      { freq : float32;
        amplitude : float32;
        mutable partials : Partial list;
        mutable fitDiff : float32; }
    and Partial =
      { sinusoid : Sinusoid;
        harmonic : Harmonic;
        multiple : int;
        decay : float32; }

  module H = HarmonicModel

  let testHarmonicModel (input : W.T) (parameters : P.T) (output : W.T) =
    let frameSize = 3000 * 2 + 1
    let hopSize = (frameSize - 1) / 4
    let fftSize = 16384 * 2
    let binSize = float32 parameters.sampleRate / float32 fftSize
    let hopTime = float32 hopSize / float32 parameters.sampleRate
    let frames = (parameters.samples + hopSize - 1) / hopSize
    let fft = FFT.create fftSize FFT.Normalization.Asymmetric
    let inputBuffer = SampleBuffer.create (W.accessor input) AccessType.ReadOnly 50000
    let outputBuffer = SampleBuffer.create (W.accessor output) AccessType.WriteOnly 50000
    let frame = Array.zeroCreate frameSize
    let fftInput = Array.zeroCreate fftSize
    let fftOutput = Array.zeroCreate fftSize
    let wf = WindowFunction.hann frameSize
    let invMaxSpectrumOfWf = 1.f / WindowFunction.maxSpectrum fftSize wf
    let c = WindowFunction.normalizationFactor wf hopSize

    let acFftInput = Array.zeroCreate fftSize
    let acFftOutput = Array.zeroCreate fftSize

    let fftSynthOutput = Array.zeroCreate fftSize

    let mutable norm = 0.f

    let fundamentalPeakCount = 10
    let maxFundamentalFreqHz = 1000.f
    let minFundamentalPeriodS = float32 parameters.sampleRate / maxFundamentalFreqHz
    let maxPartialCount = 10
    let partialDecay = 0.8f
    let fundamentalHeap = Heap.createWithCapacity fundamentalPeakCount

    let sinusoidPeakCount = 200
    let sinusoidHeap = Heap.createWithCapacity sinusoidPeakCount

    for frameIndex = 0 to frames - 1 do
      SampleBuffer.window inputBuffer wf frame
      for channel = 0 to 1 do
        FFT.sampleArrayToCentered channel frame fftInput
        FFT.compute fft fftInput fftOutput

        // Top peaks of sinusoidal model (sinusoidPeakCount)

        let topSinusoidPeaks () =
          Heap.clear sinusoidHeap

          let mutable c0 = 0.f
          let mutable c1 = 0.f
          for j = 0 to fftSize / 2 do
            let c = Complex.abs fftOutput.[j]
            if (c0 < c1) && (c1 > c) then
              if Heap.count sinusoidHeap < sinusoidPeakCount then
                Heap.add sinusoidHeap (-c1, j - 1)
              else
                let (c', _) = Heap.top sinusoidHeap
                if c1 > -c' then
                  Heap.replace sinusoidHeap (-c1, j - 1) |> ignore
            c0 <- c1
            c1 <- c

        topSinusoidPeaks ()

        // Compute autocorrelation

        for j = 0 to fftSize / 2 do
          acFftInput.[j] <- Complex.createRe (Complex.abs fftOutput.[j])
        for j = fftSize / 2 + 1 to fftSize - 1 do
          acFftInput.[j] <- acFftInput.[fftSize - j]
        FFT.computeInverse fft acFftInput acFftOutput

        // Top peaks of autocorrelation (fundamentalPeakCount)

        let topFundamentalPeaks () =
          Heap.clear fundamentalHeap

          let mutable c0 = 0.f
          let mutable c1 = 0.f
          for j = int minFundamentalPeriodS to fftSize / 2 do
            let c = Complex.re acFftOutput.[j]
            if (c0 < c1) && (c1 > c) then
              if Heap.count fundamentalHeap < fundamentalPeakCount then
                Heap.add fundamentalHeap (-c1, j - 1)
              else
                let (c', _) = Heap.top fundamentalHeap
                if c1 > -c' then
                  Heap.replace fundamentalHeap (-c1, j - 1) |> ignore
            c0 <- c1
            c1 <- c

        topFundamentalPeaks ()

        let showFundamentalPeakChart () =
          let chartLength = 1000
          let chartData =
            acFftOutput
            |> Seq.skip 1
            |> Seq.mapi (fun i z -> (float32 i, (Complex.re z) ** 0.5f))
            |> Seq.take chartLength
          let chartLabels = Array.create chartLength ""
          fundamentalHeap
          |> Heap.toArray
          |> Array.iter
            (fun (_, i) ->
              if i <= chartLength then
                chartLabels.[i - 1] <- (float32 parameters.sampleRate / float32 i).ToString("N1")
            )
          Chart.Line(chartData, Labels=chartLabels) |> showChart

        //showFundamentalPeakChart ()

        let simpleSynthOutput () =
          Array.fill fftSynthOutput 0 fftSynthOutput.Length Complex.zero
          for (_, index) in Heap.toArray fundamentalHeap do
            let freq = float32 parameters.sampleRate / float32 index
            let amplitude = Complex.abs fftOutput.[int (freq / binSize)]
            let mutable decay = 1.f
            for multiple = 1 to maxPartialCount do
              let partialFreq = freq * float32 multiple
              let partialBin = int (partialFreq / binSize)
              if partialBin < fftSize / 2 then
                fftSynthOutput.[partialBin] <-
                  Complex.add
                    fftSynthOutput.[partialBin]
                    (Complex.createRe (amplitude * invMaxSpectrumOfWf * decay))
              decay <- decay * partialDecay

        // Instrument model
        let partialDecay multiple =
          match multiple with
          | 1 -> 1.f
          | 2 -> 2.f
          | 3 -> 1.2f
          | 4 -> 0.4f
          | 5 -> 0.6f
          | 6 -> 0.9f
          | 7 -> 1.2f
          | _ -> exp (-0.1f * float32 multiple)

        let fitHarmonicTerms () =
          let freqThresholdBaseHz = 15.f
          let freqThresholdFactor = 1.2f
          let mutable sinusoids =
            Heap.toArray sinusoidHeap
            |> Array.map
              (fun (_, index) ->
                { H.Sinusoid.freq = float32 index * binSize;
                  H.Sinusoid.amplitude = Complex.abs fftOutput.[index];
                  H.Sinusoid.partials = [];
                  H.Sinusoid.fitDiff = 0.f; }
              )
            |> Array.sortBy (fun sinusoid -> sinusoid.freq)
          let tryLowerBoundIndex freq (a : H.Sinusoid array) =
            let rec loop i j =
              if i > j then
                if 0 <= i && i < a.Length && a.[i].freq >= freq then
                  Some i
                else
                  None
              else
                let mid = (i + j) / 2
                if a.[mid].freq = freq then
                  Some mid
                else if a.[mid].freq < freq then
                  loop (mid + 1) j
                else
                  loop i (mid - 1)
            loop 0 (a.Length - 1)
          let mutable zeroSinusoidList = []
          let mutable harmonics =
            Heap.toArray sinusoidHeap
            |> Array.sortBy fst
            |> Array.take 30
            //Heap.toArray fundamentalHeap
            |> Array.map
              (fun (_, index) ->
                //let freq = float32 parameters.sampleRate / float32 index
                let freq = float32 index * binSize;
                let amplitude = Complex.abs fftOutput.[int (freq / binSize)]
                let harmonic =
                  { H.Harmonic.freq = freq;
                    H.Harmonic.partials = [];
                    H.Harmonic.originalAmplitude = amplitude
                    H.Harmonic.fitAmplitude = amplitude / 10.f; }
                let mutable freqThresholdHz = freqThresholdBaseHz
                for multiple = 1 to maxPartialCount do
                  let partialFreq = harmonic.freq * float32 multiple
                  let addPartial sinusoid =
                    let partial =
                      { H.Partial.sinusoid = sinusoid;
                        H.Partial.harmonic = harmonic;
                        H.Partial.multiple = int multiple;
                        H.Partial.decay = partialDecay multiple; }
                    sinusoid.partials <- partial :: sinusoid.partials
                    harmonic.partials <- partial :: harmonic.partials
                  let addZeroPartial () =
                    let zeroSinusoid =
                      { H.Sinusoid.freq = partialFreq;
                        H.Sinusoid.amplitude = 0.f;
                        H.Sinusoid.partials = [];
                        H.Sinusoid.fitDiff = 0.f; }
                    zeroSinusoidList <- zeroSinusoid :: zeroSinusoidList
                    addPartial zeroSinusoid
                  match tryLowerBoundIndex partialFreq sinusoids with
                  | Some index ->
                    let tryAddPartial (sinusoid : H.Sinusoid) =
                      if sinusoid.freq > partialFreq - freqThresholdHz && sinusoid.freq < partialFreq + freqThresholdHz then
                        addPartial sinusoid
                        true
                      else
                        false
                    if not (tryAddPartial sinusoids.[index]) then
                      if index = 0 || not (tryAddPartial sinusoids.[index - 1]) then
                        addZeroPartial ()
                  | None ->
                    addZeroPartial ()
                  freqThresholdHz <- freqThresholdHz * freqThresholdFactor
                harmonic
              )

          harmonics <- harmonics |> Array.filter (fun harmonic -> harmonic.partials.Length <> 0)
          sinusoids <- sinusoids |> Array.filter (fun sinusoid -> sinusoid.partials.Length <> 0)
          sinusoids <- Array.append sinusoids (List.toArray zeroSinusoidList)

          let sensitivityAmplitude = 0.00001f
          let sensitivityDecay = 0.000001f
          let regularizerTotalAmplitude = 0.f

          let updateFitAndComputeLoss () =
            let mutable loss = 0.f
            for sinusoid in sinusoids do
              let mutable fitValue = 0.f
              for partial in sinusoid.partials do
                fitValue <- fitValue + partial.harmonic.fitAmplitude * partial.decay
              sinusoid.fitDiff <- fitValue - sinusoid.amplitude
              loss <- loss + sinusoid.fitDiff * sinusoid.fitDiff
            for harmonic in harmonics do
              loss <- loss + regularizerTotalAmplitude * harmonic.fitAmplitude * harmonic.fitAmplitude / harmonic.originalAmplitude
            loss

          let rec fit oldLoss =
            for harmonic in harmonics do
              let mutable derivAmplitude = 0.f
              for partial in harmonic.partials do
                derivAmplitude <- derivAmplitude + partial.sinusoid.fitDiff * partial.decay
              derivAmplitude <- derivAmplitude + regularizerTotalAmplitude * harmonic.fitAmplitude / harmonic.originalAmplitude
              harmonic.fitAmplitude <- harmonic.fitAmplitude - derivAmplitude * sensitivityAmplitude
              if harmonic.fitAmplitude < 0.f then
                harmonic.fitAmplitude <- 0.f

            let newLoss = updateFitAndComputeLoss ()

            if oldLoss - newLoss > 0.1f then
              fit newLoss
            else
              ()

          fit (updateFitAndComputeLoss ())

          harmonics

        let harmonics = fitHarmonicTerms () |> Array.sortByDescending (fun harmonic -> harmonic.fitAmplitude)

        let showSinusoidPeakChart () =
          let chartLength = 4000
          let chartData =
            fftOutput
            |> Seq.skip 1
            |> Seq.mapi (fun i z -> (float32 i * binSize, Complex.abs z))
            |> Seq.take chartLength
          let chartLabels = Array.create chartLength ""
          sinusoidHeap
          |> Heap.toArray
          |> Array.iter
            (fun (_, i) ->
              if i <= chartLength then
                chartLabels.[i - 1] <- (float32 i * binSize).ToString("N1")
            )
          let sinusoidChart = Chart.Line(chartData, Labels=chartLabels)
          let harmonicData =
            harmonics
            |> Seq.map (fun harmonic -> (harmonic.freq, harmonic.fitAmplitude * 10.f))
          let harmonicChart = Chart.Point(harmonicData, MarkerSize=10)
          Chart.Combine([sinusoidChart; harmonicChart]) |> showChart

        //showSinusoidPeakChart ()

        let harmonicSynthOutput () =
          Array.fill fftSynthOutput 0 fftSynthOutput.Length Complex.zero
          for harmonic in harmonics do
            for multiple = 1 to maxPartialCount do
              let partialFreq = harmonic.freq * float32 multiple
              let partialBin = int (partialFreq / binSize)
              if partialBin < fftSize / 2 then
                fftSynthOutput.[partialBin] <-
                  Complex.add
                    fftSynthOutput.[partialBin]
                    (Complex.createRe (harmonic.fitAmplitude * invMaxSpectrumOfWf * partialDecay multiple))

        harmonicSynthOutput ()

        for j = fftSize / 2 + 1 to fftSize - 1 do
          fftSynthOutput.[j] <- Complex.conj fftSynthOutput.[fftSize - j]

        FFT.computeInverse fft fftSynthOutput fftInput
        FFT.sampleArrayFromCentered channel frame fftInput
      SampleBuffer.add' outputBuffer c wf frame
      norm <- max norm (SampleBuffer.norm outputBuffer frameSize)
      SampleBuffer.moveBy outputBuffer hopSize
      SampleBuffer.moveBy inputBuffer hopSize
      if frameIndex % 1 = 0 then
        printf
          "\rProcessed frame %d / %d (%.1f%%)"
          (frameIndex + 1)
          frames
          (float (frameIndex + 1) / float frames * 100.)
    SampleBuffer.flush outputBuffer
    printfn ""
    printfn "Norm: %f" norm

  [<EntryPoint>]
  let main argv =
      use input = W.openFile AccessType.ReadOnly "D:\\Box\\dancing_queen.wav"
      let parameters = W.parameters input
      let output = W.createFile "D:\\Box\\test.wav" parameters

      testHarmonicModel input parameters output

      (output :> IDisposable).Dispose()

      printfn "Done."
      stdin.ReadLine() |> ignore
      0
