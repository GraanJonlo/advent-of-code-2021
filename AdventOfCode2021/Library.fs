namespace AdventOfCode2021

[<Measure>]
type depth

type Scan = int<depth> list

module Scan =
    let slidingWindow size (input: int<depth> list) =
        let rec slidingWindow' size input agg =
            if List.length input < size then
                List.rev agg
            else
                slidingWindow' size (List.tail input) (List.take size input :: agg)

        slidingWindow' size input []

    type Sampler = int<depth> list -> int<depth> list list

    let resample (sampler: Sampler) input = sampler input |> List.map List.sum

    let numberOfDepthIncreases resampler input =
        let resampledInput = resampler input

        List.fold
            (fun (lastDepth, noOfIncreases) depth ->
                if depth > lastDepth then
                    (depth, noOfIncreases + 1)
                else
                    (depth, noOfIncreases))
            (List.head resampledInput, 0)
            (List.tail resampledInput)
        |> snd
