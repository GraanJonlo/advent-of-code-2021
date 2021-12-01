namespace AdventOfCode2021

[<Measure>] type depth

type Scan = int<depth> list

type numberOfDepthIncreases = Scan -> int

module Scan =
    let numberOfDepthIncreases x =
        List.fold
            (fun (lastDepth, noOfIncreases) depth ->
                if depth > lastDepth then
                    (depth, noOfIncreases + 1)
                else
                    (depth, noOfIncreases))
            (List.head x, 0)
            (List.tail x)
        |> snd
