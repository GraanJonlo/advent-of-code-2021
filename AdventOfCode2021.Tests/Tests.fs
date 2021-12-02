module Tests

open Xunit
open FsUnit.Xunit

open AdventOfCode2021

[<Fact>]
let ``Sliding window`` () =
    [ 1<depth>
      2<depth>
      3<depth>
      1<depth> ]
    |> Scan.slidingWindow 2
    |> should
        equal
        [ [ 1<depth>; 2<depth> ]
          [ 2<depth>; 3<depth> ]
          [ 3<depth>; 1<depth> ] ]

[<Fact>]
let ``Resample`` () =
    let slidingWindow =
        fun _ ->
            [ [ 1<depth>; 2<depth> ]
              [ 2<depth>; 3<depth> ]
              [ 3<depth>; 1<depth> ] ]

    []
    |> Scan.resample slidingWindow
    |> should equal [ 3<depth>; 5<depth>; 4<depth> ]

[<Fact>]
let ``Depth increases and decreases`` () =
    let resampler =
        fun _ -> [ 3<depth>; 5<depth>; 4<depth> ]

    []
    |> Scan.numberOfDepthIncreases resampler
    |> should equal 1

[<Fact>]
let ``Calculate final position`` () =
    [ Forward 2; Forward 1; Down 2; Up 1 ]
    |> Move.finalPosition
    |> should equal (3<move>, 1<aim>)

[<Fact>]
let ``Moves forward without depth change when aim is 0`` () =
    { horizontal = 0<move>
      depth = 0<move>
      aim = 0 }
    |> Position.move (Forward 2)
    |> should
        equal
        { horizontal = 2<move>
          depth = 0<move>
          aim = 0 }

[<Fact>]
let ``Changes aim`` () =
    { horizontal = 0<move>
      depth = 0<move>
      aim = 0 }
    |> Position.move (Down 2)
    |> Position.move (Down 1)
    |> Position.move (Up 1)
    |> should
        equal
        { horizontal = 0<move>
          depth = 0<move>
          aim = 2 }

[<Fact>]
let ``Increases depth`` () =
    { horizontal = 0<move>
      depth = 0<move>
      aim = 0 }
    |> Position.updatePosition Position.move [ Down 3; Forward 2 ]
    |> should
        equal
        { horizontal = 2<move>
          depth = 6<move>
          aim = 3 }
