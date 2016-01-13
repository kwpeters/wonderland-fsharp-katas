// See the file card-game.md for detailed information.

// feel free to use these cards or use your own data structure

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
        

let rec playGame (hand1:Card list, hand2:Card list) =
    match hand1,hand2 with
    | [],_ -> printfn "Player 2 wins"
    | _,[] -> printfn "Player 1 wins"
    | c1::xs,c2::ys -> if playRound (c1,c2) then
                            printfn "Player 1 hand"
                            playGame(xs@(c1::c2::[]),ys)
                       else
                            printfn "Player 2 hand"
                            playGame(xs,ys@(c1::c2::[]))

let ranks =
    [   for v in 2 .. 10 -> Value v
        for head in heads -> head
    ]

let deck = seq {
    for suit in suits do
        for rank in ranks -> suit,rank }

let rand = new System.Random()
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

let h1,h2 = getHands deck           
playGame (h1,h2)

// fill in tests for your game
let tests () =

    // playRound
    printfn "TODO: the highest rank wins the cards in the round"
    printfn "TODO: queens are higher rank than jacks"
    printfn "TODO: kings are higher rank than queens"
    printfn "TODO: aces are higher rank than kings"
    printfn "TODO: if the ranks are equal, clubs beat spades"
    printfn "TODO: if the ranks are equal, diamonds beat clubs"
    printfn "TODO: if the ranks are equal, hearts beat diamonds"

    // playGame
    printfn "TODO: the player loses when they run out of cards"

// run the tests
tests ()
