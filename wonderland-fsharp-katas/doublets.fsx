// See the file doublets.md for detailed information.

open System.IO
open System.Text.RegularExpressions

let wordsPath = Path.Combine (__SOURCE_DIRECTORY__,"resources","words.txt")
let words = File.ReadAllLines wordsPath |> List.ofArray

type Word = string

type doubletTree = {
    Word: Word
    nodes: doubletTree list
}

let anyChar = "[a-z]"

let filterDoublets x (w: Word) i e =
    Regex.Match(x,"^"+w.Substring(0,i)+anyChar+w.Substring(i+1)+"$").Success
    && not (List.contains x e)

let buildNextDoublets (w:Word) = 
    let rec oneCharDoublet l i e =
        if i = w.Length then
            l
         else
            let founds = (List.filter (fun x -> filterDoublets x w i e) words)
            let newEx = List.append founds e
            let newFounds = List.append founds l
            oneCharDoublet newFounds (i+1) newEx
    oneCharDoublet [] 0 [w]

let findDoubletInTree r w2 = 
    let rec getList (t:doubletTree list) l =
        let current = t.Head
        if current.Word = w2 then (current.Word::l)
        else if current.nodes.Length = 0 then []
        else getList t.Tail (current.Word::l)
            
    getList r.nodes []

let buildNextLevelTree root = 
    let nextDoublets = List.map (fun w -> {Word=w;nodes=[]}) (buildNextDoublets root.Word)
    {root with nodes = nextDoublets}

let doublets (w1:Word,w2:Word) = 
    let root = {Word = w1; nodes = []}
    let rec findInTree t =
        let result = findDoubletInTree t w2
        if result.Length <> 0 then result
        else 
            let level = buildNextLevelTree t
            findInTree level
    findInTree root

#r @"../packages/Unquote/lib/net45/Unquote.dll"
open Swensen.Unquote

let tests () =

    test <@ doublets ("head", "tail") = ["head"; "heal"; "teal"; "tell"; "tall"; "tail"] @>
    test <@ doublets ("door", "lock") = ["door"; "boor"; "book"; "look"; "lock"] @>
    test <@ doublets ("bank", "loan") = ["bank"; "bonk"; "book"; "look"; "loon"; "loan"] @>
    test <@ doublets ("wheat", "bread") = ["wheat"; "cheat"; "cheap"; "cheep"; "creep"; "creed"; "breed"; "bread"] @>

    test <@ doublets ("ye", "freezer") = [] @>

// run the tests
tests ()
