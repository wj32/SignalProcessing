namespace SignalProcessing

module SampleBuffer =
  type T =
    { accessor : SampleAccessor.T;
      accessType : AccessType.T;
      size : int;
      mutable position : int;
      data : Sample.T array;
      mutable offset : int; }

  let read t toOffset fromPosition sizeToRead =
    assert (toOffset >= 0)
    let invalidStartSize = if fromPosition < 0 then min sizeToRead (-fromPosition) else 0
    let validFirstPosition = fromPosition + invalidStartSize
    let validSize =
      match t.accessType with
      | AccessType.ReadOnly | AccessType.ReadWrite ->
        if validFirstPosition < t.accessor.size then
          min (sizeToRead - invalidStartSize) (t.accessor.size - validFirstPosition)
        else
          0
      | AccessType.WriteOnly -> 0
    let invalidEndSize = sizeToRead - (invalidStartSize + validSize)
    let mutable j = toOffset % t.size
    for i = 0 to invalidStartSize - 1 do
      t.data.[j] <- Sample.zero
      j <- if j = t.size - 1 then 0 else j + 1
    for i = 0 to validSize - 1 do
      t.data.[j] <- t.accessor.get (validFirstPosition + i)
      j <- if j = t.size - 1 then 0 else j + 1
    for i = 0 to invalidEndSize - 1 do
      t.data.[j] <- Sample.zero
      j <- if j = t.size - 1 then 0 else j + 1

  let write t fromOffset toPosition sizeToWrite =
    assert (fromOffset >= 0)
    let invalidStartSize = if toPosition < 0 then min sizeToWrite (-toPosition) else 0
    let validFirstPosition = toPosition + invalidStartSize
    let validSize =
      match t.accessType with
      | AccessType.ReadOnly -> 0
      | AccessType.ReadWrite | AccessType.WriteOnly ->
        if validFirstPosition < t.accessor.size then
          min (sizeToWrite - invalidStartSize) (t.accessor.size - validFirstPosition)
        else
          0
    let mutable j = (fromOffset + invalidStartSize) % t.size
    for i = 0 to validSize - 1 do
      t.accessor.set (validFirstPosition + i) t.data.[j]
      j <- if j = t.size - 1 then 0 else j + 1

  let create accessor accessType size =
    let t =
      { accessor = accessor;
        accessType = accessType;
        size = size;
        position = 0;
        data = Array.zeroCreate size;
        offset = 0; }
    read t 0 0 size
    t

  let size t = t.size

  let position t = t.position

  let data t = t.data

  let offset t = t.offset

  let get t i = t.data.[(t.offset + i) % t.size]

  let set t i s = t.data.[(t.offset + i) % t.size] <- s

  let flush t =
    write t t.offset t.position t.size
    t.offset <- 0
    read t t.offset t.position t.size

  let moveBy t increment =
    if (increment >= t.size) || (increment <= -t.size) then
      write t t.offset t.position t.size
      t.position <- t.position + increment
      t.offset <- 0
      read t t.offset t.position t.size
    else if increment > 0 then
      let endSize = min increment (t.size - t.offset)
      write t t.offset t.position endSize
      read t t.offset (t.position + t.size) endSize
      let startSize = increment - endSize
      if startSize <> 0 then
        write t 0 (t.position + endSize) startSize
        read t 0 (t.position + endSize + t.size) startSize
      t.position <- t.position + increment
      t.offset <- (t.offset + increment) % t.size
    else if increment < 0 then
      let startSize = min (-increment) t.offset
      write t (t.offset - startSize) (t.position - startSize + t.size) startSize
      read t (t.offset - startSize) (t.position - startSize) startSize
      let endSize = (-increment) - startSize
      if endSize <> 0 then
        write t (t.size - endSize) (t.position - startSize - endSize + t.size) endSize
        read t (t.size - endSize) (t.position - startSize - endSize) endSize
      t.position <- t.position - increment
      t.offset <- (t.offset - increment + t.size) % t.size

  let moveTo t position = moveBy t (position - t.position)

  let window t (windowFunction : WindowFunction.T) (toArray : Sample.T array) =
    let width = windowFunction.width
    if width > t.size then failwith "The window width is too large."
    let mutable i = 0
    let mutable j = t.offset
    for i = 0 to width - 1 do
      toArray.[i] <- Sample.mul t.data.[j] (windowFunction.f i)
      j <- if j = t.size - 1 then 0 else j + 1

  let add t factor (fromArray : Sample.T array) =
    let mutable i = 0
    let mutable j = t.offset
    for i = 0 to fromArray.Length - 1 do
      t.data.[j] <- Sample.addMul t.data.[j] fromArray.[i] factor
      j <- if j = t.size - 1 then 0 else j + 1

  let copyTo t dst size =
    let mutable i = dst.offset
    let mutable j = t.offset
    for k = 0 to size - 1 do
      dst.data.[i] <- t.data.[j]
      i <- if i = dst.size - 1 then 0 else i + 1
      j <- if j = t.size - 1 then 0 else j + 1

  let copyToArray t (dst : Sample.T array) =
    let mutable i = 0
    let mutable j = t.offset
    for i = 0 to dst.Length - 1 do
      dst.[i] <- t.data.[j]
      j <- if j = t.size - 1 then 0 else j + 1

  let copyFromArray t (src : Sample.T array) =
    let mutable i = 0
    let mutable j = t.offset
    for i = 0 to src.Length - 1 do
      t.data.[j] <- src.[i]
      j <- if j = t.size - 1 then 0 else j + 1
