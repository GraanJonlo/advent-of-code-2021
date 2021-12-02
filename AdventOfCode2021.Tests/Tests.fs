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
    [ Forward 2<move>
      Forward 1<move>
      Down 2<move>
      Up 1<move> ]
    |> Move.finalPosition
    |> should equal (3<move>, 1<move>)
