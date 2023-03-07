module Main

open Prog

[<EntryPoint>]
let main argv =
    let t = emptyTrieNode
    printfn "%A" (t |> insert "qwe" |> insert "qas"|> insert "asd")
    0
