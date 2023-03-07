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

