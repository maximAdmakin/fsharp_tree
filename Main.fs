module Main

open Prog
open System

[<EntryPoint>]
let main argv =
    let t = (treeFromList ["qwe"; "qwzx"; "asd"])
    let t2 = (treeFromList ["sdf"])
    // let t = (treeFromList [null])
    printfn "%A" t
    printfn "%A" (rightfold (fun c s -> s + string c) "1" t)
    printfn "%A" (map (Char.ToUpper) t)
    printfn "%A" (merge t t2)

    let t = ["Sa"; "s"] |> treeFromList
    printfn "%A" t
    printfn "%A" (map (fun c -> char (int c + 1)) t)
    let leftFoldFrstRes = Seq.map Char.ToUpper (leftfold (fun c s -> s + string c) "" t) |> Seq.toArray |> String
    let mapFrstRes = (leftfold (fun c s -> s + string c) "" (map (Char.ToUpper) t))
    printfn "%A" leftFoldFrstRes
    printfn "%A" mapFrstRes
    0
