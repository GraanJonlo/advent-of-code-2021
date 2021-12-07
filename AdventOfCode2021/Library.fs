namespace AdventOfCode2021

[<Measure>]
type metre

type Scan = int<metre> list

module Scan =
    let slidingWindow size (input: Scan) =
        let rec slidingWindow' size input agg =
            if List.length input < size then
                List.rev agg
            else
                slidingWindow' size (List.tail input) (List.take size input :: agg)

        slidingWindow' size input []

    type Sampler = int<metre> list -> int<metre> list list

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

type Move =
    | Forward of int<metre>
    | Up of int
    | Down of int
    | NoMove

module Move =
    let finalPosition moves =
        moves
        |> List.fold
            (fun (oldX, oldY) m ->
                match m with
                | Forward distance -> (oldX + distance, oldY)
                | Up distance -> (oldX, oldY - distance)
                | Down distance -> (oldX, oldY + distance)
                | NoMove -> (oldX, oldY))
            (0<metre>, 0)

type Position =
    { horizontal: int<metre>
      depth: int<metre>
      aim: int }

module Position =
    let move m p =
        match m with
        | Forward dist ->
            { p with
                horizontal = p.horizontal + dist
                depth = p.depth + (p.aim * dist) }
        | Up aimDecrease -> { p with aim = p.aim - aimDecrease }
        | Down aimIncrease -> { p with aim = p.aim + aimIncrease }
        | NoMove -> p

    let updatePosition mover moves p =
        List.fold (fun p m -> mover m p) p moves
