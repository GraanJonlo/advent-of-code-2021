module Tests

open Xunit
open FsUnit.Xunit

open AdventOfCode2021

[<Fact>]
let ``Depth increases and decreases`` () =
    [ 1<depth>
      2<depth>
      3<depth>
      1<depth> ]
    |> Scan.numberOfDepthIncreases
    |> should equal 2

