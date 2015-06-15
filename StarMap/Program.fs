open Microsoft.FSharp.Core.Operators
let tuplePlus (a,b) (c,d) = 
  (a + b, c + d)
let tupleNeg (a,b) =
  (- a, -b)

(*
let calcMotion (x, y) (x', y') (x1, y1) (x1', y1') =
  //let i = sin (System.Math.PI / 2.0)
  //let mutable res = (0.0, 0.0, 0.0) // (angree i, x0, y0)
  let t = tupleNeg (x', y')
  let (x',y') = (x' - x, y' - y)
  let (x1,y1) = (x1 - x, y1 - y)
  let (x1',y1') = (x1' - x, y1' - y)
  let (x, y) = (0.0, 0.0)
  let mutable count = true
  for i in 0.0 .. 0.001 .. 2.0 * System.Math.PI do
    let x0 = x' - x * cos i + y * sin i
    let y0 = y' - x * sin i - y * cos i
    let temp = x1 * cos i - y1 * sin i + x0
    let temp2 = sin i
    let temp3 = cos i
    if (abs (x1' - (x1 * cos i - y1 * sin i + x0)) < eps) && (abs (y1' - (x1 * sin i + y1 * cos i + y0)) < eps) then 
      //res <- (i, x0, y0)
      let temp = x1 * cos i - y1 * sin i + x0
      let temp2 = sin i
      let temp3 = cos i
      count <- false
  //res
  count
  *)
let calcMotion2 (x, y) (x', y') (x1, y1) (x1', y1') eps =
  let mutable res = (0.0, 0.0, 0.0, 0.0) // (sin i, cos i, x0, y0)
  (*
  let t = tupleNeg (x, y)
  let (x',y') = tuplePlus (x', y') t
  let (x1,y1) = tuplePlus (x1, y1) t
  let (x1',y1') = tuplePlus (x1', y1') t
  let (x, y) = (0.0, 0.0)
  *)
  let t = (x,y) 
  let (x', y') = (x' - x, y' - y)
  let (x1,y1) = (x1 - x, y1 - y)
  let (x1',y1') = (x1' - x, y1' - y)
  let (x, y) = (0.0, 0.0)
  //coefficents for calculating angle via quadratic equation
  let c1 = pown (x1 - x) 2
  let c2 = pown (y1 - y) 2
  let c3 = pown (x1' - x') 2
  let c4 = 2.0 * (x1' - x') * (y - y1)
  //Calculating sin and cos 1 time is more efficient than calculating angle and then calculating sin's and cos's multiple times.
  let ang1s  = ((c4 + sqrt (pown c4 2 - 4.0 * (c1 + c2) * (c3 - c1))) / ( 2.0 * (c1 + c2))) //sin of 1st angle
  let ang1cP = sqrt (1.0 - pown ang1s 2) //Positive cos of 1st angle
  let ang1cN = - sqrt (1.0 - pown ang1s 2) //Negative cos of 1st angle
  let x0_1P = x' - x * ang1cP + y * ang1s
  let y0_1P = y' - x * ang1s - y * ang1cP
  let x0_1N = x' - x * ang1cN + y * ang1s
  let y0_1N = y' - x * ang1s - y * ang1cN

  let ang2s = ((c4 - sqrt (pown c4 2 - 4.0 * (c1 + c2) * (c3 - c1))) / ( 2.0 * (c1 + c2)))
  let ang2cP = sqrt (1.0 - pown ang2s 2)
  let ang2cN = - sqrt (1.0 - pown ang2s 2)
  let x0_2P = x' - x * ang2cP + y * ang2s
  let y0_2P = y' - x * ang2s - y * ang2cP
  let x0_2N = x' - x * ang2cN + y * ang2s
  let y0_2N = y' - x * ang2s - y * ang2cN
(*  let temp = ang1c
  let temp2 = ang1s
  let temp3 = (x1 * ang1c - y1 * ang1s + x0_1)
  let temp4= (x1 * ang1s + y1 * ang1c + y0_1)
  let temp5 = (x1 * ang2c - y1 * ang2s + x0_2)
  let temp6= (x1 * ang2s + y1 * ang2c + y0_2) *)
  if (abs (x1' - (x1 * ang1cP - y1 * ang1s + x0_1P)) < eps) && (abs (y1' - (x1 * ang1s + y1 *  ang1cP + y0_1P)) < eps) then 
      res <- (ang1s, ang1cP, x0_1P, y0_1P)
  else
    if (abs (x1' - (x1 * ang2cP - y1 * ang2s + x0_2P)) < eps) && (abs (y1' - (x1 * ang2s + y1 * ang2cP + y0_2P)) < eps) then 
      res <- (ang2s, ang2cP, x0_2P, y0_2P)
    else
      //if (abs (x1' - (x1 * ang1cN - y1 * ang1s + x0_1N)) < eps) && (abs (y1' - (x1 * ang1s + y1 *  ang1cN + y0_1N)) < eps) then 
        //  res <- (ang1s, ang1cN, x0_1N, y0_1N)
     // else
      if (abs (x1' - (x1 * ang2cN - y1 * ang2s + x0_2N)) < eps) && (abs (y1' - (x1 * ang2s + y1 * ang2cN + y0_2N)) < eps) then 
          res <- (ang2s, ang2cN, x0_2N, y0_2N)
  res

let stringToPoint (s : string) =
  let mutable x = ""
  let mutable y = ""
  let mutable i = 0
  while (i < s.Length) && (s.[i] <> ' ') do
    x <- x + s.[i].ToString()
    i <- i + 1

  i <- i + 1

  while (i < s.Length) && (s.[i] <> ' ') do
    y <- y + s.[i].ToString()
    i <- i + 1
  ((float) x, (float) y)

let length (a : float ,b : float) (c : float,d : float)  = 
  sqrt (pown (a - c) 2 + pown (b - d) 2)

let motionPoint (x,y) sin cos x0 y0 (xshift, yshift) =
  let x = x - xshift
  let y = y - yshift 
  let x' = x * cos - y * sin + x0 + xshift
  let y' = x * sin + y * cos + y0 + yshift
  (x', y')
  
[<EntryPoint>]
let main argv = 
  (*
  let (sin, cos, x0, y0) = calcMotion2 (3.0,1.0) (20.0,6.0) (3.0,3.0) (20.0,4.0) 0.1
  let xshift = 3.0
  let yshift = 1.0
  printfn "%A" (motionPoint (1.0,1.0) sin cos x0 y0 (xshift, yshift))
  *)
  
  use in1 = new System.IO.StreamReader ("in.txt")
  let pointDelta = (float) (in1.ReadLine ())
  let arcDelta = (float) (in1.ReadLine ())
  let mutable buffer = in1.ReadLine ()
  let mutable pointsBuf = []
  while (buffer <> "Next map") do
    let point = stringToPoint buffer
    pointsBuf <- point :: pointsBuf
    buffer <- in1.ReadLine ()
  let map1 = List.toArray pointsBuf //List will be reversed, but it doesn't matter for us. Next times too.
    //'map' stands for 'star map'
  pointsBuf <- []
  buffer <- in1.ReadLine ()
  while (buffer <> null) do
    let point = stringToPoint buffer
    pointsBuf <- point :: pointsBuf
    buffer <- in1.ReadLine ()
  let map2 = Array.sort (List.toArray pointsBuf )

  let mutable arcBuffer = []
  for i = 0 to map1.Length - 1 do
   for j = i + 1 to map1.Length - 1 do
    arcBuffer <- (length map1.[i] map1.[j], (map1.[i], map1.[j])) :: arcBuffer
      //Complicated structure (length, (point, pont)) is used so that 'fst' and 'snd' commands could be used
  let arcs1 = List.toArray arcBuffer

  arcBuffer <- []
  for i = 0 to map2.Length - 1 do
   for j = i + 1 to map2.Length - 1 do
    arcBuffer <- (length map2.[i] map2.[j], (map2.[i], map2.[j])) :: arcBuffer
  let arcs2 = Array.sort (List.toArray arcBuffer)
  
  let res = ref 0
  for arc in arcs1 do
    let mutable m = arcs2.Length / 2
    let arcs2len = arcs2.Length
    let mutable mChange = 1
    while abs(fst arc- fst arcs2.[m]) > arcDelta && mChange <> 0 do
      let mOld = m
      if (fst arc < fst arcs2.[m]) then m <- m / 2
      else m <- m + (arcs2len - m) / 2
      mChange <- mOld - m
    let sndArc = ref m
    while !sndArc > 0 && abs(fst arc - fst arcs2.[!sndArc]) < arcDelta do
      sndArc.Value <- !sndArc - 1
    if (abs(fst arc - fst arcs2.[!sndArc]) > arcDelta) then sndArc.Value <- !sndArc + 1

    while !sndArc < arcs2.Length && abs(fst arc - fst arcs2.[!sndArc]) < arcDelta do
      let (sin, cos, x0, y0) = calcMotion2 (fst (snd arc)) (fst (snd arcs2.[!sndArc])) (snd (snd arc)) (snd (snd arcs2.[!sndArc])) pointDelta
      let functio (sin, cos, x0, y0) (xshift, yshift) = 
        let map2len = map2.Length
        let usedPoints = Array.create map2len false
        let mutable count = 0
        for i in map1 do
          let fstPoint = motionPoint i sin cos x0 y0 (xshift, yshift)
          let mutable mpoint = map2.Length / 2
          let mutable mpointChange = 1
          while abs (fst fstPoint - fst map2.[mpoint]) > pointDelta  && mpointChange <> 0 do
            let mpointOld = mpoint
            if (fst fstPoint < fst map2.[mpoint]) then mpoint <- mpoint / 2
            else mpoint <- mpoint + (map2len - mpoint) / 2
            mpointChange <- mpointOld - mpoint
          while abs (fst fstPoint - fst map2.[mpoint]) < pointDelta && mpoint > 0 do
            mpoint <- mpoint - 1
          if (abs (fst fstPoint - fst map2.[mpoint]) > pointDelta) then mpoint <- mpoint + 1
          let mutable notFoundFlag = true
          while mpoint < map2.Length && abs (fst fstPoint - fst map2.[mpoint]) < pointDelta && notFoundFlag do
            if length fstPoint map2.[mpoint] < pointDelta && not usedPoints.[mpoint] then
              count <- count + 1
              usedPoints.[mpoint] <- true
              notFoundFlag <- false
            mpoint <- mpoint + 1
        if (count > !res) then res.Value <- count
        sndArc.Value <- !sndArc + 1
      functio (sin, cos, x0, y0) (fst (snd arc))
      if (!sndArc < arcs2.Length) then
        let (sin, cos, x0, y0) = calcMotion2 (snd (snd arc)) (fst (snd arcs2.[!sndArc])) (fst (snd arc)) (snd (snd arcs2.[!sndArc])) pointDelta
        functio (sin, cos, x0, y0) (snd (snd arc))
  printfn "%A" !res
  
  (*
  for i in map1 do
    printf "%A " i
  printfn ""
  for i in map2 do
    printf "%A " i
    *)
(*  for i in -50.0 ..  50.0 do
    for j in -50.0 ..  50.0 do
      for i1 in -50.0 ..  50.0 do
        for j1 in -50.0 ..  50.0 do *)
  (*
  let swatch = new System.Diagnostics.Stopwatch ()
  swatch.Start ()
  let i = 1.0
  let j = 1.0
  let i1 = 33.0
  let j1 = 32.0
  for i2 in -20.0 ..  20.0 do
    for j2 in -20.0 ..  20.0 do
      for i3 in -200.0 ..  20.0 do
        for j3 in -20.0 ..  50.0 do
          
          if abs (pown (i - i2) 2 + pown (j - j2) 2 -  (pown (i1 - i3) 2 + pown (j1 - j3) 2)) < eps then
            let a = calcMotion (i,j) (i1, j1) (i2, j2) (i3, j3)
            if a then
              c <- c + 1
              printfn "%A %A %A %A " i2 j2 i3 j3 
  swatch.Stop()
  printfn "%A" swatch.Elapsed
  *)
  //let r = calcMotion2 (1.0, 1.0) (33.0, 32.0) (1.0, -19.0) (17.0, 20.0)
  //let r1 = calcMotion (1.0, 1.0) (33.0, 32.0) (0.0, -20.0) (14.0, 41.0)
  //let r = calcMotion2 (1.0, 1.0) (33.0, 32.0) (0.0, -20.0) (14.0, 41.0)
  //printfn "%A" r
 // printfn "%A" (calcMotion (1.0,1.0) (3.0,3.0) (3.0,3.0) (1.0, 50.0))
  0
