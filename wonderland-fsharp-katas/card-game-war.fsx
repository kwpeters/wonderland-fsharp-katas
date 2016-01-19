// See the file card-game.md for detailed information.

// feel free to use these cards or use your own data structure
open System.Collections.Generic

type Suit =
    | Spade
    | Club
    | Diamond
    | Heart

type Rank =
    | Value of int
    | Jack
    | Queen
    | King
    | Ace

type Card = Suit * Rank

let suits = [ Spade; Club; Diamond; Heart ]
let heads = [ Jack; Queen; King; Ace ]

let rand = new System.Random()

let ranks =
    [   for v in 2 .. 10 -> Value v
        for head in heads -> head
    ]

let deck = seq {
    for suit in suits do
        for rank in ranks -> suit,rank }

type Hands = Card list*Card list
 
let drop i l =
    let rec drop' i l1 p l2 =
        if List.length l2 = List.length l1 - 1 then l2
        else 
            let p = if p = i then p+1  else p 
            drop' i l1 (p+1) (List.append l2 [(List.item p l1)])
    drop' i l 0 [] 

let getCard d cs =
    let i = rand.Next(0, List.length d)
    let h1Cards = d.[i]::cs
    h1Cards,drop i d

let getHands dec =
    let rec getHandAccs d (h1,h2) =
        if List.length d = 0 then (h1,h2)
        else 
            let h1Cards, d1 = getCard d h1
            let h2Cards, d2 = getCard d1 h2
            getHandAccs d2 (h1Cards,h2Cards)
    getHandAccs (Seq.toList dec) ([],[])

let playRound (card1:Card,card2:Card) =
    let greaterIndexFirst e1 e2 l =
        List.findIndex (fun r -> r = e1) l > List.findIndex (fun r -> r = e2) l
        
    match card1,card2 with
    | (s1,Value x), (s2,Value y) -> 
        if x = y then greaterIndexFirst s1 s2 suits
        else x > y
    | (s1,Value x), (s2,_) -> false
    | (s1,_),(s2,Value y) -> true
    | (s1,r1),(s2,r2) -> 
        if r1 = r2 then 
            greaterIndexFirst s1 s2 suits
        else greaterIndexFirst r1 r2 heads 
       

let playGame (hand1:Card list, hand2:Card list) max =
    let rec playGame' h1 h2 handNum =
        match h1,h2 with
        | [],_ -> sprintf "Player 2 wins"// after %i hands" handNum
        | _,[] -> sprintf "Player 1 wins"// after %i hands" handNum
        | c1::xs,c2::ys ->  if handNum = max then
                                sprintf "Infinite loop detected after %i hands: DRAW" max
                            else
                                //printfn "The hand number %i is %A" max (c1,c2)
                                if playRound (c1,c2) then      
                                    //printfn "Player 1 wins and it has %i cards" ((List.length xs) + 2)
                                    //printfn "Player 1 has %i cards" (List.length ys)
                                    playGame' (xs@(c1::c2::[])) ys (handNum+1)
                                else
                                    //printfn "Player 2 wins and it has %i cards" ((List.length ys) + 2)
                                    //printfn "Player 1 has %i cards" (List.length xs)
                                    playGame' xs (ys@(c1::c2::[])) (handNum+1)
    playGame' hand1 hand2 1

let h1,h2 = getHands deck           
//playGame (List.take 8 h1,List.take 8 h2)

let h1Test = [(Heart, Value 2); (Heart, Value 3); (Club, Value 4); (Diamond, Value 5);
                (Heart, Value 6); (Spade, Value 7); (Club, Value 8); (Diamond, Value 9);]

let h2Test = [(Diamond, Jack); (Diamond, Queen); (Heart, King); (Club, Ace);
                (Heart, Jack); (Diamond, Value 2); (Heart, Queen); (Spade, King);]

playGame (h1Test,h2Test) 100
//playGame (h1,h2) 10000000

#r @"../packages/Unquote/lib/net45/Unquote.dll"
open Swensen.Unquote

// fill in tests for your game
let tests () =

    // playRound
    test <@ playRound((Heart, Value 2), (Heart, Value 3)) = false @>
    test <@ playRound((Club, Value 3), (Spade, Value 2)) = true @>
    test <@ playRound((Club, Value 3), (Spade, King)) = false @>
    test <@ playRound((Club, Queen), (Spade, King)) = false @>
    test <@ playRound((Club, Ace), (Spade, Value 2)) = true @>
    test <@ playRound((Club, Value 3), (Spade, Value 3)) = true @>
    test <@ playRound((Diamond, Ace), (Heart, Ace)) = false @>

    // playGame
    test <@ playGame (h1Test, h2Test) 100 = "Player 2 wins" @>
    test <@ playGame (h2Test, h1Test) 100 = "Player 1 wins" @>

// run the tests
tests ()

