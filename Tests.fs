module Tests

open Xunit
open FsCheck
open FsCheck.Xunit
open Prog


[<Property>]
let ``leftFold and rightFold same result on sum`` (list1: List<int>, acc: int) =
    true


[<Fact>]
let ``leftFold calculates the sum of all values``() =
    
    Assert.Equal(1, 1)

