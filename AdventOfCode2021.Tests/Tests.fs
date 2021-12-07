module Tests

open Xunit
open FsUnit.Xunit

open AdventOfCode2021

[<Fact>]
let ``Sliding window`` () =
    [ 1<metre>
      2<metre>
      3<metre>
      1<metre> ]
    |> Scan.slidingWindow 2
    |> should
        equal
        [ [ 1<metre>; 2<metre> ]
          [ 2<metre>; 3<metre> ]
          [ 3<metre>; 1<metre> ] ]

[<Fact>]
let ``Resample`` () =
    let slidingWindow =
        fun _ ->
            [ [ 1<metre>; 2<metre> ]
              [ 2<metre>; 3<metre> ]
              [ 3<metre>; 1<metre> ] ]

    []
    |> Scan.resample slidingWindow
    |> should equal [ 3<metre>; 5<metre>; 4<metre> ]

[<Fact>]
let ``Depth increases and decreases`` () =
    let resampler =
        fun _ -> [ 3<metre>; 5<metre>; 4<metre> ]

    []
    |> Scan.numberOfDepthIncreases resampler
    |> should equal 1

[<Fact>]
let ``Calculate final position`` () =
    [ Forward 2<metre>; Forward 1<metre>; Down 2; Up 1 ]
    |> Move.finalPosition
    |> should equal (3<metre>, 1)

[<Fact>]
let ``Moves forward without depth change when aim is 0`` () =
    { horizontal = 0<metre>
      depth = 0<metre>
      aim = 0 }
    |> Position.move (Forward 2<metre>)
    |> should
        equal
        { horizontal = 2<metre>
          depth = 0<metre>
          aim = 0 }

[<Fact>]
let ``Changes aim`` () =
    { horizontal = 0<metre>
      depth = 0<metre>
      aim = 0 }
    |> Position.move (Down 2)
    |> Position.move (Down 1)
    |> Position.move (Up 1)
    |> should
        equal
        { horizontal = 0<metre>
          depth = 0<metre>
          aim = 2 }

[<Fact>]
let ``Increases depth`` () =
    { horizontal = 0<metre>
      depth = 0<metre>
      aim = 0 }
    |> Position.updatePosition Position.move [ Down 3; Forward 2<metre> ]
    |> should
        equal
        { horizontal = 2<metre>
          depth = 6<metre>
          aim = 3 }

[<Fact>]
let ``Find most common bits/ gamma`` () =
    [ Vector [ 1; 1; 1 ]
      Vector [ 1; 0; 1 ]
      Vector [ 0; 0; 0 ] ]
    |> Vector.gamma
    |> should equal <| Vector [ 1; 0; 1 ]

[<Fact>]
let ``Find least common bits/ epsilon`` () =
    [ Vector [ 1; 1; 1 ]
      Vector [ 1; 0; 1 ]
      Vector [ 0; 0; 0 ] ]
    |> Vector.epsilon
    |> should equal <| Vector [ 0; 1; 0 ]
