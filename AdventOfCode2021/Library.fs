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

[<Measure>]
type move

[<Measure>]
type aim

type Move =
    | Forward of int
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
            (0, 0)

type Position =
    { horizontal: int<move>
      depth: int<move>
      aim: int }

module Position =
    let move m p =
        match m with
        | Forward dist ->
            { p with
                horizontal = p.horizontal + (dist * 1<move>)
                depth = p.depth + (p.aim * dist * 1<move>) }
        | Up aimDecrease -> { p with aim = p.aim - aimDecrease }
        | Down aimIncrease -> { p with aim = p.aim + aimIncrease }
        | NoMove -> p

    let updatePosition mover moves p =
        List.fold (fun p m -> mover m p) p moves
