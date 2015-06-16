let calcMotion (x, y) (x', y') (x1, y1) (x1', y1') eps =
  let mutable res = (0.0, 0.0, 0.0, 0.0) // (sin of angle, cos of angle, shift on X axis, shift on Y axis)
  //Motion that moves one arc to another of similar size always exists, so initial contents of res don't matter

  //moving coordinates for rotation:
  let (x', y') = (x' - x, y' - y)
  let (x1,y1) = (x1 - x, y1 - y)
  let (x1',y1') = (x1' - x, y1' - y)
  let (x, y) = (0.0, 0.0)

  //coefficents for calculating angle via quadratic equation
  //we basically have a system of equasions:
  // x'  = x * cos (angle) - y * sin (angle) + x0,
  // x1' = x1 * cos (angle) - y1 * sin (angle) + x0
  // We exclude x0, square, change cos ^ 2 to 1 - sin ^ 2 and get quadratic equasion. 
  //And then we solve it via discriminant and get ang1s and ang2s
  let c1 = pown (x1 - x) 2
  let c2 = pown (y1 - y) 2
  let c3 = pown (x1' - x') 2
  let c4 = 2.0 * (x1' - x') * (y - y1)
  //Calculating sin and cos 1 time is more efficient than calculating angle and then calculating sinuses and cosinuses multiple times.
  let ang1s  = ((c4 + sqrt (pown c4 2 - 4.0 * (c1 + c2) * (c3 - c1))) / ( 2.0 * (c1 + c2))) //sin of 1st angle
  let ang1cP = sqrt (1.0 - pown ang1s 2)   //Positive cos of 1st angle
  let ang1cN = - sqrt (1.0 - pown ang1s 2) //Negative cos of 1st angle
  let x0_1P = x' - x * ang1cP + y * ang1s  //shift on X axis with positive cos of 1st angle
  let y0_1P = y' - x * ang1s - y * ang1cP  //shift on X axis with negative cos of 1st angle
  let x0_1N = x' - x * ang1cN + y * ang1s  //shift on Y axis with positive cos of 1st angle
  let y0_1N = y' - x * ang1s - y * ang1cN  //shift on Y axis with negative cos of 1st angle

  let ang2s  = ((c4 - sqrt (pown c4 2 - 4.0 * (c1 + c2) * (c3 - c1))) / ( 2.0 * (c1 + c2)))
  let ang2cP = sqrt (1.0 - pown ang2s 2)
  let ang2cN = - sqrt (1.0 - pown ang2s 2)
  let x0_2P = x' - x * ang2cP + y * ang2s
  let y0_2P = y' - x * ang2s - y * ang2cP
  let x0_2N = x' - x * ang2cN + y * ang2s
  let y0_2N = y' - x * ang2s - y * ang2cN

  if (abs (x1' - (x1 * ang1cP - y1 * ang1s + x0_1P)) < eps) 
    && (abs (y1' - (x1 * ang1s + y1 *  ang1cP + y0_1P)) < eps) then 
      res <- (ang1s, ang1cP, x0_1P, y0_1P)
  else
    if (abs (x1' - (x1 * ang2cP - y1 * ang2s + x0_2P)) < eps) 
      && (abs (y1' - (x1 * ang2s + y1 * ang2cP + y0_2P)) < eps) then 
       res <- (ang2s, ang2cP, x0_2P, y0_2P)
    else
      if (abs (x1' - (x1 * ang1cN - y1 * ang1s + x0_1N)) < eps) 
        && (abs (y1' - (x1 * ang1s + y1 *  ang1cN + y0_1N)) < eps) then 
          res <- (ang1s, ang1cN, x0_1N, y0_1N)
      else
        if (abs (x1' - (x1 * ang2cN - y1 * ang2s + x0_2N)) < eps) 
          && (abs (y1' - (x1 * ang2s + y1 * ang2cN + y0_2N)) < eps) then 
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
  while (i < s.Length) do
    y <- y + s.[i].ToString()
    i <- i + 1
  ((float) x, (float) y)

let arcLength (a : float, b : float) (c : float,d : float)  = 
  sqrt (pown (a - c) 2 + pown (b - d) 2)

let motionPoint (x,y) sin cos x0 y0 (xshift, yshift) =
  let x = x - xshift
  let y = y - yshift 
  let x' = x * cos - y * sin + x0 + xshift
  let y' = x * sin + y * cos + y0 + yshift
  (x', y')
  
[<EntryPoint>]
let main argv = 
  //Input file should go like this: 
  //pointDelta
  //arcDelta
  //x1 y1 \n ... xk yk \n       //Points from first star map
  //Next map
  //x1' y1' \n ... xl' yl' \n   //Points from second star map
  use in1 = new System.IO.StreamReader ("in.txt")
  let pointDelta = (float) (in1.ReadLine ())
  let arcDelta = (float) (in1.ReadLine ())
  let mutable buffer = in1.ReadLine ()
  let mutable pointsBuf = [] //I use list and then transform it into array 'cause adding to the list is faster and number of points is unknown
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
  let map2 = Array.sort (List.toArray pointsBuf ) //sorting is asymptotically profitable

  if (map1.Length = 1) || (map2.Length = 1) then printfn "1"
  else
    let mutable arcBuffer = []
    for i = 0 to map1.Length - 1 do
     for j = i + 1 to map1.Length - 1 do
      arcBuffer <- (arcLength map1.[i] map1.[j], (map1.[i], map1.[j])) :: arcBuffer
        //Complicated structure (length, (point, pont)) is used so that 'fst' and 'snd' commands could be used
    let arcs1 = List.toArray arcBuffer

    arcBuffer <- []
    for i = 0 to map2.Length - 1 do
     for j = i + 1 to map2.Length - 1 do
      arcBuffer <- (arcLength map2.[i] map2.[j], (map2.[i], map2.[j])) :: arcBuffer
    let arcs2 = Array.sort (List.toArray arcBuffer)
  
    let mutable res = 0
    for arc in arcs1 do
      let arcs2len = arcs2.Length
      let mutable sndArc = arcs2len / 2
      let mutable sndArcChange = 1 //any number that isn't zero will do.
      //finding arcs nearly similar in length:
      while abs(fst arc- fst arcs2.[sndArc]) > arcDelta && sndArcChange <> 0 do
        let sndArcOld = sndArc
        if (fst arc < fst arcs2.[sndArc]) then sndArc <- sndArc / 2
        else sndArc <- sndArc + (arcs2len - sndArc) / 2
        sndArcChange <- sndArcOld - sndArc
      while sndArc > 0 && abs(fst arc - fst arcs2.[sndArc]) < arcDelta do
        sndArc <- sndArc - 1
      if (abs(fst arc - fst arcs2.[sndArc]) > arcDelta) then sndArc <- sndArc + 1

      while sndArc < arcs2.Length && abs(fst arc - fst arcs2.[sndArc]) < arcDelta do
      
        let countMatchingPoints (sin, cos, x0, y0) (xshift, yshift) = 
          let map2len = map2.Length
          let usedPoints = Array.create map2len false
          let mutable count = 0
          for i in map1 do
            let fstPoint = motionPoint i sin cos x0 y0 (xshift, yshift)
            let mutable sndPoint = map2len / 2
            let mutable sndPointChange = 1 //any number that isn't zero will do
            //finding points that are closer than pointDelta (only on X axis! At this time we aren't looking at Y axis, we'll check that later)
            while abs (fst fstPoint - fst map2.[sndPoint]) > pointDelta  && sndPointChange <> 0 do
              let sndPointOld = sndPoint
              if (fst fstPoint < fst map2.[sndPoint]) then sndPoint <- sndPoint / 2
              else sndPoint <- sndPoint + (map2len - sndPoint) / 2
              sndPointChange <- sndPointOld - sndPoint
            while abs (fst fstPoint - fst map2.[sndPoint]) < pointDelta && sndPoint > 0 do
              sndPoint <- sndPoint - 1
            if (abs (fst fstPoint - fst map2.[sndPoint]) > pointDelta) then sndPoint <- sndPoint + 1

            let mutable notFoundFlag = true
            while sndPoint < map2.Length && abs (fst fstPoint - fst map2.[sndPoint]) < pointDelta && notFoundFlag do
              if arcLength fstPoint map2.[sndPoint] < pointDelta && not usedPoints.[sndPoint] then
                count <- count + 1
                usedPoints.[sndPoint] <- true
                notFoundFlag <- false
              sndPoint <- sndPoint + 1
          count

        //For each arc we find 2 motions: 1 moves beginning of first arc to the beginning of second arc, and end to end
        //Another moves beginning of 1st to end of 2nd, and end of 1st to beginning of 2nd
        let (sin, cos, x0, y0) = calcMotion (fst (snd arc)) (fst (snd arcs2.[sndArc])) (snd (snd arc)) (snd (snd arcs2.[sndArc])) pointDelta
        let curRes1 = countMatchingPoints (sin, cos, x0, y0) (fst (snd arc))
        if  (res < curRes1) then res <- curRes1

        let (sin, cos, x0, y0) = calcMotion (snd (snd arc)) (fst (snd arcs2.[sndArc])) (fst (snd arc)) (snd (snd arcs2.[sndArc])) pointDelta
        let curRes2 = countMatchingPoints (sin, cos, x0, y0) (snd (snd arc))
        if (res < curRes2) then res <- curRes2
        sndArc <- sndArc + 1

    printfn "%A" res
  0
