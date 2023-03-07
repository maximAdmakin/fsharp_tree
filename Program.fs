module Prog

type TrieNode =
    { IsEndOfWord: bool
      Children: Map<char, TrieNode> }

let emptyTrieNode =
    { IsEndOfWord = false
      Children = Map.empty }

let rec insert (word: string) (trieNode: TrieNode) =
    match word with
    | null -> { trieNode with IsEndOfWord = true }
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

let rec map f (trieNode: TrieNode) =
    let mapChild (acc: Map<char,TrieNode>) (k: char) (v: TrieNode) =
        let newKey = f k
        let newValue = map f v
        acc.Add(newKey, newValue)
    
    let mappedChildren = trieNode.Children |> Map.fold mapChild Map.empty<char, TrieNode>
    
    { IsEndOfWord = trieNode.IsEndOfWord;
      Children = mappedChildren }

let rec leftfold f state trieNode =
    trieNode.Children 
    |> Map.fold (fun acc char childNode -> leftfold f acc childNode |> f char) state

let rec rightfold f state trieNode =
    trieNode.Children 
    |> Map.fold (fun acc char childNode -> rightfold f acc childNode |> f char) state

let treeFromList list = List.fold (fun tree x -> insert x tree) emptyTrieNode list

let rec merge (t1: TrieNode) (t2: TrieNode) =
    let mergedChildren =
        Seq.append (t1.Children |> Map.toSeq) (t2.Children |> Map.toSeq)
        |> Seq.groupBy fst
        |> Seq.map (fun (c, childSeq) ->
            let mergedChild = childSeq |> Seq.map snd |> Seq.reduce merge
            c, mergedChild)
        |> Map.ofSeq
    { IsEndOfWord = t1.IsEndOfWord || t2.IsEndOfWord
      Children = mergedChildren }
