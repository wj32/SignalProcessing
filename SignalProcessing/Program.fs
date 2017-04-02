namespace SignalProcessing

open System

module Main =
  module W = WaveFile
  module P = W.Parameters

  let testDiff (input : W.T) (parameters : P.T) (output : W.T) =
    for i = 0 to parameters.samples - 1 do
      let sample = W.get input i
      let diff = sample.x1 / 2s - sample.x2 / 2s
      let sample' = Sample.create diff diff
      W.set output i sample'

  let testModAmp (input : W.T) (parameters : P.T) (output : W.T) =
    let modHz = 1.
    for i = 0 to parameters.samples - 1 do
      let sample = W.getF input i
      let u = float i / float parameters.sampleRate
      let c = 0.5 * (1. + sin (u * 2. * Math.PI * modHz))
      let sample' = sample * c
      W.setF output i sample'

  [<EntryPoint>]
  let main argv =
      use input = W.openFile W.Access.ReadOnly "D:\\Box\\dancing_queen.wav"
      let parameters = W.parameters input
      let output = W.createFile "D:\\Box\\test.wav" parameters

      testModAmp input parameters output

      (output :> IDisposable).Dispose()

      printfn "Done."
      stdin.ReadLine() |> ignore
      0
