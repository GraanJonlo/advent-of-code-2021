namespace AdventOfCode2021

open System

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

type Vector<'a> = Vector of 'a list

module Vector =
    let unwrap (Vector v) = v

    let add (Vector v1) (Vector v2) =
        List.zip v1 v2
        |> List.map (fun (x, y) -> x + y)
        |> Vector

    let sum = List.reduce add

    let map fn (Vector v) =
        List.map fn v
        |> Vector

    let parseInt radix (Vector v) =
        let rec parseInt' radix n total remains =
            match remains with
            | [] -> total
            | head :: tail ->
            parseInt' radix (n * radix) (total + (head * n)) tail

        List.rev v
        |> parseInt' radix 1 0

    let gamma vs =
        vs
        |> sum
        |> map float
        |> map (fun x -> x / (float <| List.length vs))
        |> map (fun x -> Math.Round(x))
        |> map int

    let epsilon vs =
        gamma vs
        |> map (fun x -> x - 1)
        |> map (fun x -> Math.Abs(x))
