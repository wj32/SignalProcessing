namespace SignalProcessing

open System
open System.IO
open System.IO.MemoryMappedFiles

module WaveFile =
  module SA = SampleAccessor

  module Parameters =
    type T =
      { channels : int; (* sample/msample *)
        bitDepth : int; (* bit/sample *)
        sampleRate : int; (* msample/second *)
        samples : int; (* msample *) }

  exception InvalidFormatException of message : string

  type T =
    { file : MemoryMappedFile;
      view : MemoryMappedViewAccessor;
      dataOffset : int64;
      channels : int; (* sample/msample *)
      bitDepth : int; (* bit/sample *)
      sampleRate : int; (* msample/second *)
      samples : int; (* msample *)
      bytesPerSample: int; (* byte/msample *)
      byteRate : int; (* byte/second *)
      accessor : SampleAccessor.T; }

    interface IDisposable with
      member t.Dispose() =
        t.view.Dispose()
        t.file.Dispose()

  module Format =
    module AudioFormat =
      let [<Literal>] Pcm = 1s

    module ChunkId =
      let [<Literal>] Data = 0x61746164
      let [<Literal>] Fmt = 0x20746d66
      let [<Literal>] Riff = 0x46464952

    module FormatId =
      let [<Literal>] Wave = 0x45564157

  let parameters t =
    { Parameters.channels = t.channels;
      Parameters.bitDepth = t.bitDepth;
      Parameters.sampleRate = t.sampleRate;
      Parameters.samples = t.samples; }

  let accessor t = t.accessor

  let get t i = t.accessor.get i

  let set t i s = t.accessor.set i s

  let getI t i = t.accessor.getI i

  let setI t i s = t.accessor.setI i s

  let createAccessor (view : MemoryMappedViewAccessor) b channels bitDepth samples =
    match channels, bitDepth with
    | 1, 16 ->
      { SA.size = samples;
        SA.get = (fun i -> {x1 = (float32 (view.ReadInt16(b + int64 (i * 2)))) / 32768.f; x2 = 0.f});
        SA.set = (fun i s -> view.Write(b + int64 (i * 2), int16 (round (s.x1 * 32768.f))));
        SA.getI = (fun i -> {x1 = view.ReadInt16(b + int64 (i * 2)); x2 = 0s});
        SA.setI = (fun i s -> view.Write(b + int64 (i * 2), int16 s.x1)); }
    | 2, 16 ->
      { SA.size = samples;
        SA.get =
          (fun i ->
            let v = view.ReadInt32(b + int64 (i * 4))
            {x1 = float32 (int16 (v &&& 0xffff)) / 32768.f; x2 = float32 (int16 (v >>> 16)) / 32768.f}
          );
        SA.set =
          (fun i s ->
            let v = (int (round (s.x1 * 32768.f)) &&& 0xffff) + (int (round (s.x2 * 32768.f)) <<< 16)
            view.Write(b + int64 (i * 4), int32 v)
          );
        SA.getI =
          (fun i ->
            let v = view.ReadInt32(b + int64 (i * 4))
            {x1 = int16 (v &&& 0xffff); x2 = int16 (v >>> 16)}
          );
        SA.setI = (fun i s -> view.Write(b + int64 (i * 4), int32 (int s.x1 + (int s.x2 <<< 16)))); }
    | _ -> raise (InvalidFormatException "Unsupported channel and bit depth combination")

  let openFile accessType fileName =
    let memoryMappedFileAccess =
      match accessType with
      | AccessType.ReadOnly -> MemoryMappedFileAccess.Read
      | AccessType.ReadWrite -> MemoryMappedFileAccess.ReadWrite
      | AccessType.WriteOnly -> invalidArg "accessType" "WriteOnly is not valid for opening a file"
    let file =
      MemoryMappedFile.CreateFromFile(fileName, FileMode.Open, null, 0L, memoryMappedFileAccess)
    let view = file.CreateViewAccessor(0L, 0L, memoryMappedFileAccess)

    // Header chunk
    let headerChunkId = view.ReadInt32(0L)
    let totalSize = 8 + view.ReadInt32(4L)
    let headerFormatId = view.ReadInt32(8L)
    if headerChunkId <> Format.ChunkId.Riff then
      raise (InvalidFormatException "Invalid RIFF header chunk ID")
    if headerFormatId <> Format.FormatId.Wave then
      raise (InvalidFormatException "Invalid RIFF header format ID")

    // Format chunk
    let formatChunkId = view.ReadInt32(12L)
    let audioFormat = view.ReadInt16(20L)
    let channels = int (view.ReadInt16(22L))
    let sampleRate = view.ReadInt32(24L)
    let byteRate = view.ReadInt32(28L)
    let blockAlign = int (view.ReadInt16(32L))
    let bitDepth = int (view.ReadInt16(34L))
    if audioFormat <> Format.AudioFormat.Pcm then
      raise (InvalidFormatException "Invalid audio format")
    if bitDepth % 8 <> 0 then
      raise (InvalidFormatException "Invalid bits depth")
    let bytesPerSample = (channels * bitDepth + 7) / 8 // Round up
    if blockAlign <> bytesPerSample then
      raise (InvalidFormatException "Invalid block alignment")

    // Data chunk
    let rec findDataChunk baseOffset =
      if baseOffset + 8L > int64 totalSize then
        raise (InvalidFormatException "Unable to find data chunk")
      let chunkId = view.ReadInt32(baseOffset)
      let dataSize = view.ReadInt32(baseOffset + 4L)
      let baseOffset' = baseOffset + 8L + int64 dataSize
      if chunkId <> Format.ChunkId.Data then
        findDataChunk baseOffset'
      else
        (baseOffset + 8L, dataSize)
    let dataOffset, dataSize = findDataChunk 36L

    let samples = dataSize / bytesPerSample
    { file = file;
      view = view;
      dataOffset = dataOffset;
      channels = channels;
      bitDepth = bitDepth;
      sampleRate = sampleRate;
      samples = samples;
      bytesPerSample = bytesPerSample;
      byteRate = bytesPerSample * sampleRate;
      accessor = createAccessor view dataOffset channels bitDepth samples; }

  let createFile fileName (parameters : Parameters.T) =
    let bytesPerSample = (parameters.channels * parameters.bitDepth + 7) / 8 // Round up
    let byteRate = bytesPerSample * parameters.sampleRate
    let dataSize = bytesPerSample * parameters.samples

    let totalSize = 44L + int64 dataSize
    let file =
      MemoryMappedFile.CreateFromFile(
        fileName,
        FileMode.Create,
        null,
        totalSize,
        MemoryMappedFileAccess.ReadWrite
      )
    let view = file.CreateViewAccessor(0L, 0L, MemoryMappedFileAccess.ReadWrite)

    // Header chunk
    view.Write(0L, int32 Format.ChunkId.Riff)
    view.Write(4L, int32 (totalSize - 8L))
    view.Write(8L, int32 Format.FormatId.Wave)

    // Format chunk
    view.Write(12L, int32 Format.ChunkId.Fmt)
    view.Write(16L, int32 16)
    view.Write(20L, int16 Format.AudioFormat.Pcm)
    view.Write(22L, int16 parameters.channels)
    view.Write(24L, int32 parameters.sampleRate)
    view.Write(28L, int32 byteRate)
    view.Write(32L, int16 bytesPerSample)
    view.Write(34L, int16 parameters.bitDepth)

    // Data chunk
    view.Write(36L, int32 Format.ChunkId.Data)
    view.Write(40L, int32 dataSize)
    let dataOffset = 44L

    { file = file;
      view = view;
      dataOffset = dataOffset;
      channels = parameters.channels;
      bitDepth = parameters.bitDepth;
      sampleRate = parameters.sampleRate;
      samples = parameters.samples;
      bytesPerSample = bytesPerSample;
      byteRate = byteRate;
      accessor =
        createAccessor view dataOffset parameters.channels parameters.bitDepth parameters.samples; }
