## Лабораторная №2

<b>Выполнил:</b> Адмакин Максим Александрович \
<b>Группа:</b> p34102 \
<b>Преподаватель:</b> Пенской Александр Владимирович

### Двоичное дерево поиска на F#
главный тип TrieNode бывает либо IsEndOfWord либо промежуточной нодой. Строка представляеться путём в дереве.

```
(t |> insert "qwe" |> insert "qas"|> insert "asd")
{ IsEndOfWord = false
  Children =
   map
     [('a',
       { IsEndOfWord = false
         Children =
          map [('s', { IsEndOfWord = false
                       Children = map [('d', { IsEndOfWord = true
                                               Children = map [] })] })] });
      ('q',
       { IsEndOfWord = false
         Children =
          map
            [('a', { IsEndOfWord = false
                     Children = map [('s', { IsEndOfWord = true
                                             Children = map [] })] });
             ('w', { IsEndOfWord = false
                     Children = map [('e', { IsEndOfWord = true
                                             Children = map [] })] })] })] }
```

```f#
module Prog

type TrieNode =
    { IsEndOfWord: bool
      Children: Map<char, TrieNode> }

let emptyTrieNode =
    { IsEndOfWord = false
      Children = Map.empty }

let rec insert (word: string) (trieNode: TrieNode) =
    match word with
    | "" -> { trieNode with IsEndOfWord = true }
    | _ ->
        let (head, tail) = (word.[0], word.Substring(1))
        let child = 
            match Map.tryFind head trieNode.Children with
            | Some(childNode) -> childNode
            | None -> emptyTrieNode
        let newChild = insert tail child
        { trieNode with Children = trieNode.Children.Add(head, newChild) }

let rec search (word: string) (trieNode: TrieNode) =
    match word with
    | "" -> trieNode.IsEndOfWord
    | _ ->
        let (head, tail) = (word.[0], word.Substring(1))
        match Map.tryFind head trieNode.Children with
        | Some(childNode) -> search tail childNode
        | None -> false

let rec startsWith (prefix: string) (trieNode: TrieNode) =
    match prefix with
    | "" -> true
    | _ ->
        let (head, tail) = (prefix.[0], prefix.Substring(1))
        match Map.tryFind head trieNode.Children with
        | Some(childNode) -> startsWith tail childNode
        | None -> false

let rec map (f: 'T -> 'U) (trieNode: TrieNode) =
    { IsEndOfWord = trieNode.IsEndOfWord;
      Children = trieNode.Children |> Map.map (fun _ child -> map f child) }

let rec leftfold f state trieNode =
    trieNode.Children 
    |> Map.fold (fun acc char childNode -> leftfold f acc childNode |> f char) state

let rec rightfold f state trieNode =
    trieNode.Children 
    |> Map.fold (fun _ char childNode -> rightfold f state childNode |> f char) state
    |> f ' '


```

### Тесты
PBT тесты при помощи FsCheck

```f#
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

```


```
Passed!  - Failed:     0, Passed:     4, Skipped:     0, Total:     4, Duration: 194 ms - Laba2.dll (net7.0)

```

### Выводы
В ходе лабораторной работы я плотно поработал с f#, а так же научилися использовать property-based testing и осознал эффективность такого подхода. Узнали знания, сделали задачи, вывели выводы.
