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

let filterDoublets x (w: Word) i =
    Regex.Match(x,"^"+w.Substring(0,i)+anyChar+w.Substring(i+1)+"$").Success

let rec buildWordList t =
    List.fold (fun acc node -> buildWordList node @ acc) [t.Word] t.nodes

let buildNextDoublets t root = 
    let word = t.Word
    let rec oneCharDoublet lws i filteredWords =
        if i = word.Length then lws
         else
            let founds = List.filter (fun x -> filterDoublets x word i) filteredWords
            let newWords = List.filter (fun w -> List.contains w founds |> not) filteredWords
            let newFounds = List.append founds lws
            oneCharDoublet newFounds (i+1) newWords

    let excludeWords = buildWordList root
    let newWords = List.filter (fun w -> List.contains w excludeWords |> not) words
    oneCharDoublet [] 0 newWords |> List.map (fun w -> {Word=w;nodes=[]})

let findDoubletInTree r w2 = 
    let rec getList t l =
        if t.Word = w2 then t.Word::l
        else if t.nodes.Length = 0 then []
        else
            let localAcc = t.Word::l
            let listResults = List.map (fun node -> getList node localAcc) t.nodes
            let resOpt = listResults |> List.filter (fun n -> n.Length > 0) |> List.tryHead
            match resOpt with
            | None -> []
            | Some x -> x
                
    getList r [] |> List.rev

let buildNextLevelTree root =
    let rec buildNextLevel t = 
        if t.nodes.Length = 0 then 
            {Word=t.Word; nodes = buildNextDoublets t root}
        else 
            {Word=t.Word; nodes = List.map (fun n -> buildNextLevel n) t.nodes}

    buildNextLevel root

let doublets (w1:Word,w2:Word) = 
    let root = {Word = w1; nodes = []}
    let rec findInTree t =
        let result = findDoubletInTree t w2
        if result.Length <> 0 then result
        else 
            let level = buildNextLevelTree t
            if t = level then [] // Not any more levels, result not found
            else  findInTree level
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
