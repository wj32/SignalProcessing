namespace SignalProcessing

module Heap =
  let [<Literal>] HeapEmpty = "The heap is empty."

  type T<'a when 'a : comparison> =
    { mutable data : 'a array;
      mutable count : int; }

  let create () =
    { data = Array.zeroCreate 2;
      count = 0; }

  let createWithCapacity capacity =
    { data = Array.zeroCreate capacity;
      count = 0; }

  let ensureCapacity t requiredCapacity =
    if t.data.Length < requiredCapacity then
      let newCapacity = max (t.data.Length * 2) requiredCapacity
      let newData = Array.zeroCreate newCapacity
      Array.blit t.data 0 newData 0 t.count
      t.data <- newData

  let inline swap (x : _ byref) (y : _ byref) =
    let t = x
    x <- y
    y <- t

  let rec siftDown t i =
    let j = i * 2 + 1
    if j < t.count then
      if t.data.[i] < t.data.[j] then
        swap &t.data.[i] &t.data.[j]
        siftDown t j
      else if j + 1 < t.count then
        if t.data.[i] < t.data.[j + 1] then
          swap &t.data.[i] &t.data.[j + 1]
          siftDown t (j + 1)

  let rec siftUp t j =
    if j > 0 then
      let i = (j - 1) / 2
      if t.data.[i] < t.data.[j] then
        swap &t.data.[i] &t.data.[j]
        siftUp t i

  let add t x =
    ensureCapacity t (t.count + 1)
    t.data.[t.count] <- x
    siftUp t t.count
    t.count <- t.count + 1

  let inline removeGeneric t none some =
    if t.count = 0 then
      none ()
    else
      let x = t.data.[0]
      t.count <- t.count - 1
      t.data.[0] <- t.data.[t.count]
      siftDown t 0
      some x

  let remove t = removeGeneric t (fun () -> invalidOp HeapEmpty) id

  let tryRemove t = removeGeneric t (fun () -> None) Some

  let inline replaceGeneric t y none some =
    if t.count = 0 then
      none ()
    else
      let x = t.data.[0]
      t.data.[0] <- y
      siftDown t 0
      some x

  let replace t y = replaceGeneric t y (fun () -> invalidOp HeapEmpty) id

  let tryReplace t y = replaceGeneric t y (fun () -> None) Some

  let clear t = t.count <- 0

  let count t = t.count

  let isEmpty t = t.count = 0

  let toArray t = Array.take t.count t.data
