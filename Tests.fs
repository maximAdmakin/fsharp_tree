module Tests

open Xunit
open FsCheck
open FsCheck.Xunit
open Prog
open System


[<Property>]
let ``associativity of merge`` (list1: List<string>, list2: List<string>, list3: List<string>) =
    let tree1 = treeFromList list1
    let tree2 = treeFromList list2
    let tree3 = treeFromList list3

    let a = merge tree1 (merge tree2 tree3)
    let b = merge (merge tree1 tree2) tree3
    let afold = (leftfold (fun c s -> s + string c) "" a)
    let bfold = (leftfold (fun c s -> s + string c) "" b)
    afold = bfold


[<Property>]
let ``leftFold and rightFold same result on sum`` (list1: List<string>, acc: string) =
    let t = list1 |> treeFromList
    let leftFoldRes = (leftfold (fun c s -> s + string c) acc t)
    let rightFoldRes = (rightfold (fun c s -> s + string c) acc t)
    leftFoldRes = rightFoldRes

[<Property>]
let ``leftFold and map 2 path`` (list1: List<string>) =
    let fu = (fun c -> char (int c + 1))
    let t = list1 |> treeFromList
    let leftFoldFrstRes = Seq.map fu (leftfold (fun c s -> s + string c) "" t) |> Seq.toArray |> Array.sort
    let mapFrstRes = (leftfold (fun c s -> s + string c) "" (map fu t)).ToCharArray() |> Array.sort
    leftFoldFrstRes = mapFrstRes

[<Fact>]
let ``leftFold calculates the sum of all values``() =
    let t = ["qwe"; "qwzx"; "asd"] |> treeFromList
    let leftFoldRes = (leftfold (fun c s -> s + string c) "1" t)
    Assert.Equal(leftFoldRes, "1dsaexzwq")

